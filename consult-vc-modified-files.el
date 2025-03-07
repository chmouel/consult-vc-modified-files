;;; consult-vc-modified-files.el --- Show git modified files with consult and vc  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chmouel Boudjnah

;; Author: Chmouel Boudjnah <chmouel@chmouel.com>
;; Keywords: vc, convenience
;; Created: 2023
;; Version: 0.0.8
;; Package-Requires: ((emacs "28.1") (consult "0.9"))
;; Keywords: convenience
;; Homepage: https://github.com/chmouel/consult-vc-modified-files
;;
;; This file is not part of GNU Emacs.
;;
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

;; Show modified and added files from a Git repository and show the files from
;; the HEAD commits as another sources with consult open the file with
;; find-files

;;; Code:
(require 'consult)
(require 'vc-git)

(defun consult-vc-files--git-preview (command preview-buffer-name &optional args)
  "Create preview function for git files using COMMAND.
PREVIEW-BUFFER-NAME is the name for the preview buffer.
Optional ARGS are additional arguments to pass to the git command.
Uses the same buffer management approach as `consult--buffer-preview`."
  (let ((orig-buf (window-buffer (consult--original-window)))
        (orig-prev (copy-sequence (window-prev-buffers)))
        (orig-next (copy-sequence (window-next-buffers)))
        (orig-bl (copy-sequence (frame-parameter nil 'buffer-list)))
        (orig-bbl (copy-sequence (frame-parameter nil 'buried-buffer-list)))
        (preview-buffer nil)
        other-win)
    (lambda (action cand)
      (pcase action
        ('return
         ;; Restore buffer list for the current tab
         (set-frame-parameter nil 'buffer-list orig-bl)
         (set-frame-parameter nil 'buried-buffer-list orig-bbl)
         (when preview-buffer
           (kill-buffer preview-buffer)
           (setq preview-buffer nil)))
        ('exit
         (when other-win
           (set-window-prev-buffers other-win orig-prev)
           (set-window-next-buffers other-win orig-next))
         (when preview-buffer
           (kill-buffer preview-buffer)
           (setq preview-buffer nil)))
        ('preview
         ;; Clean up existing preview buffer if needed
         (when preview-buffer
           (kill-buffer preview-buffer)
           (setq preview-buffer nil))
         ;; Create and display preview for valid candidates
         (when (and cand (not (string-empty-p cand)))
           ;; Prevent opening the preview in another tab
           (cl-letf* (((symbol-function #'display-buffer-in-tab) #'ignore)
                      ((symbol-function #'display-buffer-in-new-tab) #'ignore))
             ;; Setup preview window if using other-window display
             (when (and (eq consult--buffer-display #'switch-to-buffer-other-window)
                        (not other-win))
               (switch-to-buffer-other-window orig-buf 'norecord)
               (setq other-win (selected-window)))

             ;; Create the preview buffer
             (let ((win (or other-win (selected-window)))
                   (default-directory (vc-git-root default-directory)))
               (setq preview-buffer (get-buffer-create preview-buffer-name))
               (with-current-buffer preview-buffer
                 (setq buffer-read-only nil)
                 (erase-buffer)
                 ;; Execute the git command with the provided arguments
                 (apply #'vc-git-command preview-buffer t nil
                        command
                        (append args (list "--" cand)))
                 (goto-char (point-min))
                 (diff-mode)  ;; Apply diff-mode for syntax highlighting
                 (setq buffer-read-only t))

               ;; Display the preview buffer in the appropriate window
               (when (window-live-p win)
                 (with-selected-window win
                   (unless (or orig-prev orig-next)
                     (setq orig-prev (copy-sequence (window-prev-buffers))
                           orig-next (copy-sequence (window-next-buffers))))
                   (switch-to-buffer preview-buffer 'norecord)))))))))))

;; Define the specialized versions using the unified function
(defun consult-vc-modified-files--git-show-preview ()
  "Create preview function for file history, using vc-git for show."
  (consult-vc-files--git-preview "show" "*git-show-preview*"))

(defun consult-vc-modified-files--git-diff-preview ()
  "Create preview function for modified files.
Uses vc-git diff for modified files."
  (consult-vc-files--git-preview "diff" "*vc-diff-preview*"))

(defun consult-vc-modified-files--new-file-preview ()
  "Create preview function for new (untracked) files.
Uses consult--file-preview for new files."
  (consult--file-preview))

(defun consult-vc-modified-files--git-diff-cached-preview ()
  "Create preview function for staged files, using vc-git for diff --cached."
  (consult-vc-files--git-preview "diff" "*vc-diff-staged-preview*" '("--cached")))

(defcustom consult-vc-modified-files-sources
  '(consult-vc-modified-files-source-files
    consult-vc-modified-files-source-added-files
    consult-vc-modified-files-source-staged-files
    consult-vc-modified-files-source-head-files)
  "Sources for modified, added, staged, and HEAD files in the current Git repository.

This variable defines the file sources used by `consult-vc-modified-files`.
You can customize this list to add or remove sources as needed."
  :type '(repeat (choice (const :tag "Modified locally" consult-vc-modified-files-source-files)
                         (const :tag "Added (untracked)" consult-vc-modified-files-source-added-files)
                         (const :tag "Staged for commit" consult-vc-modified-files-source-staged-files)
                         (const :tag "Modified in HEAD" consult-vc-modified-files-source-head-files)))
  :group 'consult-vc-modified)

(defface consult-vc-modified-files-head-files-face
  '((t :inherit shadow))
  "Face for files modified in HEAD.")

(defface consult-vc-modified-files-face
  '((t :inherit default))
  "Face for locally modified files.")

(defface consult-vc-modified-files-added-face
  '((t :inherit font-lock-variable-name-face))
  "Face for new (untracked) files.")

(defface consult-vc-modified-files-staged-face
  '((t :inherit success))
  "Face for files staged for commit.")

(defvar consult-vc-modified-files-history nil
  "History for `consult-vc-modified-files`.")

(defvar consult-vc-modified-files-source-head-files
  `( :name "Modified in HEAD"
     :category file
     :narrow   ?\h
     :face consult-vc-modified-files-head-files-face
     :history consult-vc-modified-files-history
     :state ,#'consult-vc-modified-files--git-show-preview
     :items (lambda () (consult-vc-modified-files-get-files "diff-tree" "-z" "--no-commit-id" "--name-only" "-r" "HEAD"))))

(defvar consult-vc-modified-files-source-files
  `( :name "Modified locally"
     :category file
     :face consult-vc-modified-files-face
     :narrow   ?\l
     :history consult-vc-modified-files-history
     :state ,#'consult-vc-modified-files--git-diff-preview
     :items (lambda () (consult-vc-modified-files-get-files "ls-files" "-z" "-m"))))

(defvar consult-vc-modified-files-source-added-files
  `( :name "Added files"
     :category file
     :face consult-vc-modified-files-added-face
     :narrow   ?\a
     :history consult-vc-modified-files-history
     :state ,#'consult-vc-modified-files--new-file-preview
     :items (lambda () (consult-vc-modified-files-get-files "ls-files" "-z" "-o" "--exclude-standard"))))

(defvar consult-vc-modified-files-source-staged-files
  `( :name "Staged for commit"
     :category file
     :face consult-vc-modified-files-staged-face
     :narrow   ?\c
     :history consult-vc-modified-files-history
     :state    ,#'consult-vc-modified-files--git-diff-cached-preview
     :items (lambda () (consult-vc-modified-files-get-files "diff" "-z" "--cached" "--name-only"))))

(defun consult-vc-modified-files-get-files (&rest args)
  "Run a Git command with ARGS and return the output as a list of files."
  (when-let ((vc-git-root default-directory))
    (split-string
     (apply #'vc-git--run-command-string "" args) "\0" t)))

;;;###autoload
(defun consult-vc-modified-files (&optional sources)
  "Prompt user to select a modified file from the repository and open it.
SOURCES defaults to `consult-vc-modified-files-sources`."
  (interactive)
  (when-let* ((default-directory (vc-git-root default-directory))
              (selected (consult--multi (or sources consult-vc-modified-files-sources)
                                        :prompt "Choose a file: "
                                        :history 'consult-vc-modified-files-history
                                        :category 'file
                                        :sort nil)))
    (if (plist-get (cdr selected) :match)
        (find-file (car selected)))))

(provide 'consult-vc-modified-files)
;;; consult-vc-modified-files.el ends here
