;;; consult-vc-modified-files.el --- Show git modified files in a vc project with consult  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chmouel Boudjnah

;; Author: Chmouel Boudjnah <chmouel@chmouel.com>
;; Keywords: vc

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

(defvar consult--vc-modified-files-history nil)
(defun consult-vc-modified-files (file)
  "Select a modified FILE in a project in git with consult."
  (interactive
   (list
    (let* ((default-directory (project-root (project-current)))
           (modif-files (split-string (vc-git--run-command-string nil "ls-files" "-z" "-m") "\0" t)))
      (consult--read
       modif-files
       :prompt "Modified File: "
       :require-match t
       :category 'vc
       :history 'consult--vc-modified-files-history))))
  (find-file (concat (project-root (project-current)) file)))

(provide 'consult-vc-modified-files)
;;; consult-vc-modified-files.el ends here
