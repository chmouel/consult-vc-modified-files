;;; consult-vc-modified-files.el --- Show git modified files in a vc project with consult  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chmouel Boudjnah

;; Author: Chmouel Boudjnah <chmouel@chmouel.com>
;; Keywords: vc, convenience
;; Created: 2023
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (consult "0.9"))
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

;; Simply show modified files from a Git repository to select and with consult
;; and open with find-files

;;; Code:
(require 'consult)
(require 'project)

(defvar consult--source-vc-head-files
  `(:name "Modified in HEAD"
          :category vc
          :face     consult-vc-head-files
          :history  consult--vc-modified-files-history
          :items
          (lambda () (split-string
                      (vc-git--run-command-string nil "diff-tree" "-z"  "--no-commit-id" "--name-only" "-r" "HEAD") "\0" t))))

(defvar consult--source-vc-modified-files
  `(:name "Modified locally"
          :category vc
          :face     consult-vc-modified-files
          :history  consult--vc-modified-files-history
          :items
          (lambda () (split-string
                      (vc-git--run-command-string nil "ls-files" "-z" "-m" "-o" "--exclude-standard") "\0" t))))

(defface consult-vc-head-files
  '((t :inherit shadow))
  "Face used to highlight grep context in `consult-vc-head-files'.")

(defface consult-vc-modified-files
  '((t))
  "Face used to highlight buffers in `consult-vc-modified-files'.")

(defvar consult--vc-modified-files-source
  '(consult--source-vc-modified-files
    consult--source-vc-head-files))

(defun consult-vc-modified-files (&optional sources)
  (interactive)
  (let ((default-directory (project-root (project-current)))
        (selected (consult--multi (or sources consult--vc-modified-files-source)
                                  :prompt "Choose a file: "
                                  :history 'consult--vc-modified-files-history
                                  :sort nil)))
    (if (plist-get (cdr selected) :match)
        (message (concat (project-root (project-current)) (car selected))))))

(provide 'consult-vc-modified-files)
;;; consult-vc-modified-files.el ends here
