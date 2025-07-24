;;; consult-vc-modified-files-tests.el --- Tests for consult-vc-modified-files  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Chmouel Boudjnah <chmouel@chmouel.com>
;; Keywords: vc, convenience, tests

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for the consult-vc-modified-files package

;;; Code:

(require 'ert)
(require 'consult-vc-modified-files)
(require 'consult)
(require 'cl-lib)

;; Test helper functions
(defvar consult-vc-modified-files-test-dir nil
  "Temporary directory for git tests.")

(defun consult-vc-modified-files-tests--setup-git-repo ()
  "Create a temporary git repository for testing."
  (setq consult-vc-modified-files-test-dir (make-temp-file "consult-vc-test-" t))
  (let ((default-directory consult-vc-modified-files-test-dir))
    ;; Initialize git repo
    (call-process "git" nil nil nil "init")
    ;; Configure git (needed for the first commit)
    (call-process "git" nil nil nil "config" "user.name" "Test User")
    (call-process "git" nil nil nil "config" "user.email" "test@example.com")
    
    ;; Create some files and make initial commit
    (dolist (file '("file1.txt" "file2.txt" "subdir/file3.txt"))
      (let ((full-path (expand-file-name file default-directory)))
        (make-directory (file-name-directory full-path) t)
        (with-temp-file full-path
          (insert "Initial content"))))
    
    ;; Add and commit initial files
    (call-process "git" nil nil nil "add" ".")
    (call-process "git" nil nil nil "commit" "-m" "Initial commit")
    
    ;; Start fresh - clean working directory
    (call-process "git" nil nil nil "reset" "--hard" "HEAD")
    
    ;; 1. Modify file1.txt but don't stage it
    (with-temp-file (expand-file-name "file1.txt" default-directory)
      (insert "Modified content"))
    
    ;; 2. Create a new untracked file
    (with-temp-file (expand-file-name "new-file.txt" default-directory)
      (insert "New file content"))
    
    ;; 3. Modify file2.txt and stage it (AND ONLY THIS ONE)
    (with-temp-file (expand-file-name "file2.txt" default-directory)
      (insert "Modified and staged content"))
    (call-process "git" nil nil nil "add" "file2.txt")

    ;; Return the directory
    default-directory))

(defun consult-vc-modified-files-tests--teardown-git-repo ()
  "Delete the temporary git repository."
  (when (and consult-vc-modified-files-test-dir
             (file-exists-p consult-vc-modified-files-test-dir))
    (delete-directory consult-vc-modified-files-test-dir t)))

;; Mock functions for testing
(defvar consult-vc-modified-files-test-git-output nil
  "Mock output for git commands in tests.")

(defun consult-vc-modified-files-tests--mock-git-output (command &rest args)
  "Mock function for vc-git--run-command-string for COMMAND with ARGS."
  (with-temp-buffer
    (pcase command
      ("ls-files"
       (cond
        ((member "-m" args) (insert "file1.txt\0"))
        ((and (member "-o" args) (member "--exclude-standard" args)) (insert "new-file.txt\0"))
        (t nil)))
      ("diff"
       (when (member "--cached" args)
         (insert "file2.txt\0")))
      ("diff-tree"
       (when (member "HEAD" args)
         (insert "subdir/file3.txt\0"))))
    (buffer-string)))

;; Test cases

(ert-deftest consult-vc-modified-files-test-get-files ()
  "Test the function that gets files from git."
  (cl-letf (((symbol-function 'vc-git--run-command-string)
             (lambda (buffer command &rest args)
               (pcase command
                 ("ls-files"
                  (cond
                   ((member "-m" args) "file1.txt\0")
                   ((and (member "-o" args) (member "--exclude-standard" args)) "new-file.txt\0")
                   (t "")))
                 ("diff"
                  (when (member "--cached" args)
                    "file2.txt\0"))
                 ("diff-tree"
                  (when (member "HEAD" args)
                    "subdir/file3.txt\0"))
                 (_ "")))))
    ;; Test modified files
    (let ((modified (consult-vc-modified-files-get-files "ls-files" "-z" "-m")))
      (should (member "file1.txt" modified))
      (should (= (length modified) 1)))
    
    ;; Test new files
    (let ((new-files (consult-vc-modified-files-get-files "ls-files" "-z" "-o" "--exclude-standard")))
      (should (member "new-file.txt" new-files))
      (should (= (length new-files) 1)))
    
    ;; Test staged files
    (let ((staged (consult-vc-modified-files-get-files "diff" "-z" "--cached" "--name-only")))
      (should (member "file2.txt" staged))
      (should (= (length staged) 1)))))

(ert-deftest consult-vc-modified-files-test-sources ()
  "Test that all sources are properly defined."
  (should (boundp 'consult-vc-modified-files-source-files))
  (should (boundp 'consult-vc-modified-files-source-added-files))
  (should (boundp 'consult-vc-modified-files-source-staged-files))
  (should (boundp 'consult-vc-modified-files-source-head-files))
  
  ;; Check that each source has the required properties
  (dolist (source '(consult-vc-modified-files-source-files
                    consult-vc-modified-files-source-added-files
                    consult-vc-modified-files-source-staged-files
                    consult-vc-modified-files-source-head-files))
    (let ((source-val (symbol-value source)))
      (should (plist-get source-val :name))
      (should (plist-get source-val :category))
      (should (plist-get source-val :face))
      (should (plist-get source-val :narrow))
      (should (plist-get source-val :history))
      (should (plist-get source-val :state))
      (should (plist-get source-val :items)))))

(ert-deftest consult-vc-modified-files-test-preview-functions ()
  "Test that preview functions are properly defined."
  ;; Test that all preview functions exist and return a function
  (should (functionp (consult-vc-modified-files--git-show-preview)))
  (should (functionp (consult-vc-modified-files--git-diff-preview)))
  (should (functionp (consult-vc-modified-files--new-file-preview)))
  (should (functionp (consult-vc-modified-files--git-diff-cached-preview))))

(ert-deftest consult-vc-modified-files-test-with-mock-git ()
  "Test sources with mocked git output."
  (cl-letf (((symbol-function 'consult-vc-modified-files-get-files)
             (lambda (command &rest args)
               (pcase command
                 ("ls-files"
                  (cond
                   ((member "-m" args) '("file1.txt"))
                   ((and (member "-o" args) (member "--exclude-standard" args)) '("new-file.txt"))
                   (t nil)))
                 ("diff"
                  (when (member "--cached" args)
                    '("file2.txt")))
                 ("diff-tree"
                  (when (member "HEAD" args)
                    '("subdir/file3.txt")))
                 (_ nil))))
            ((symbol-function 'project-current) 
             (lambda (&optional _) t))
            ((symbol-function 'project-root) 
             (lambda (&optional _) default-directory))
            ((symbol-function 'vc-git-root) 
             (lambda (&optional _) t)))
    
    ;; Test modified files source
    (let ((files (funcall (plist-get consult-vc-modified-files-source-files :items))))
      (should (equal files '("file1.txt"))))
    
    ;; Test added files source
    (let ((files (funcall (plist-get consult-vc-modified-files-source-added-files :items))))
      (should (equal files '("new-file.txt"))))
    
    ;; Test staged files source
    (let ((files (funcall (plist-get consult-vc-modified-files-source-staged-files :items))))
      (should (equal files '("file2.txt"))))
    
    ;; Test HEAD files source
    (let ((files (funcall (plist-get consult-vc-modified-files-source-head-files :items))))
      (should (equal files '("subdir/file3.txt"))))))

(ert-deftest consult-vc-modified-files-test-customization ()
  "Test customization variables."
  (should (boundp 'consult-vc-modified-files-sources))
  (should (listp consult-vc-modified-files-sources))
  (should (memq 'consult-vc-modified-files-source-files consult-vc-modified-files-sources))
  (should (memq 'consult-vc-modified-files-source-added-files consult-vc-modified-files-sources))
  (should (memq 'consult-vc-modified-files-source-staged-files consult-vc-modified-files-sources))
  (should (memq 'consult-vc-modified-files-source-head-files consult-vc-modified-files-sources)))

(ert-deftest consult-vc-log-select-files-test ()
  "Test selecting a commit and opening one of its files."
  (let ((order 0)
        opened)
    (cl-letf (((symbol-function 'vc-git-root)
               (lambda (dir) 
                 (expand-file-name (or dir default-directory))))
              ((symbol-function 'vc-git--run-command-string)
               (lambda (&rest args)
                 (pcase args
                   (`("" "log" "--pretty=format:%h %s" "--date-order")
                    "abc123 Commit one\n")
                   (`("" "diff-tree" "-z" "--no-commit-id" "--name-only" "-r" "abc123")
                    "file1.txt\0"))))
              ((symbol-function 'consult--read)
               (lambda (cands &rest args)
                 (setq order (1+ order))
                 (car cands)))
              ((symbol-function 'find-file)
               (lambda (file) (setq opened file))))
      (consult-vc-log-select-files)
      (should (equal opened "file1.txt")))))

;; Integration test (commented out by default as it requires user interaction)
;; Uncomment to run manually
;; (ert-deftest consult-vc-modified-files-test-interactive ()
;;   "Test the interactive function (requires manual verification)."
;;   (let ((default-directory (consult-vc-modified-files-tests--setup-git-repo)))
;;     (unwind-protect
;;         (progn
;;           ;; Run the interactive function - you'll need to verify the UI manually
;;           (call-interactively 'consult-vc-modified-files))
;;       (consult-vc-modified-files-tests--teardown-git-repo))))

(provide 'consult-vc-modified-files-tests)
;;; consult-vc-modified-files-tests.el ends here
