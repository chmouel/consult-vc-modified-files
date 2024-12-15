;;; consult-vc-modified-files.el --- Show git modified files in a vc project with consult  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chmouel Boudjnah

;; Author: Chmouel Boudjnah <chmouel@chmouel.com>
;; Keywords: vc, convenience
;; Created: 2023
;; Version: 0.0.2
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
(require 'project)
(require 'vc-git)

(defcustom consult-vc-modified-files-sources
  '(consult-vc-modified-files-source-files
    consult-vc-modified-files-source-head-files)
  "Sources for modified and HEAD files in the current Git project.

This variable defines the file sources used by `consult-vc-modified-files`.
You can customize this list to add or remove sources as needed."
  :type '(repeat (choice (const :tag "Modified locally" consult-vc-modified-files-source-files)
                         (const :tag "Modified in HEAD" consult-vc-modified-files-source-head-files)))
  :group 'consult-vc-modified)

(defface consult-vc-modified-files-head-files-face
  '((t :inherit shadow))
  "Face for files modified in HEAD.")

(defface consult-vc-modified-files-face
  '((t :inherit default))
  "Face for locally modified files.")

(defvar consult-vc-modified-files-history nil
  "History for `consult-vc-modified-files`.")

(defvar consult-vc-modified-files-source-head-files
  `( :name "Modified in HEAD"
     :category vc
     :face consult-vc-modified-files-head-files-face
     :history consult-vc-modified-files-history
     :items (lambda () (consult-vc-modified-files-get-files "diff-tree" "-z" "--no-commit-id" "--name-only" "-r" "HEAD"))))

(defvar consult-vc-modified-files-source-files
  `( :name "Modified locally"
     :category vc
     :face consult-vc-modified-files-face
     :history consult-vc-modified-files-history
     :items (lambda () (consult-vc-modified-files-get-files "ls-files" "-z" "-m" "-o" "--exclude-standard"))))

(defun consult-vc-modified-files-get-files (&rest args)
  "Run a Git command with ARGS and return the output as a list of files."
  (let ((default-directory (project-root (project-current t))))
    (when (vc-git-root default-directory)
      (split-string
       (apply #'vc-git--run-command-string "" args) "\0" t))))

;;;###autoload
(defun consult-vc-modified-files (&optional sources)
  "Prompt user to select a modified file from the project and open it.
SOURCES defaults to `consult-vc-modified-files-sources`."
  (interactive)
  (let ((project (project-current t)))
    (if project
        (let* ((default-directory (project-root project))
               (selected (consult--multi (or sources consult-vc-modified-files-sources)
                                         :prompt "Choose a file: "
                                         :history 'consult-vc-modified-files-history
                                         :sort nil)))
          (if selected
              (find-file (expand-file-name (car selected) default-directory))
            (message "No files selected or no modifications found.")))
      (error "No project found"))))

(provide 'consult-vc-modified-files)
;;; consult-vc-modified-files.el ends here
