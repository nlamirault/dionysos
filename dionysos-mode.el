;;; dionysos-mode.el --- Dionysos mode

;; Copyright (C) 2015 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'dired)

(defmacro with-dionysos-buffer (&rest body)
  "Execute the forms in BODY in Dionysos buffer."
  (declare (indent 0) (debug t))
  `(with-current-buffer
       (if (bongo-buffer-p)
           (current-buffer)
         (bongo-buffer))
     ,@body))


(defun dionysos-list-directory (directory-name)
  "Insert a new directory `DIRECTORY-NAME' into the dionysos buffer."
  (interactive (list (expand-file-name
                      (read-directory-name
                       "Insert directory: " default-directory nil t
                       (when (eq major-mode 'dired-mode)
                         (when (file-directory-p (dired-get-filename))
                           (dired-get-filename t)))))))
  (when (not (file-directory-p directory-name))
    (error "Not a directory: %s" directory-name))
  (dolist (filename (directory-files directory-name t "\\`[^.]"))
    (message "File: %s" filename)))



(provide 'dionysos-mode)
;;; dionysos-mode.el ends here
