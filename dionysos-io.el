;;; dionysos-io.el --- Dionysos input/output tools

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

(require 'f)

(defun dionysos--list-directory (directory-name &optional filter)
  "Insert a new directory `DIRECTORY-NAME' into the dionysos buffer."
  (interactive (list (expand-file-name
                      (read-directory-name
                       "Insert directory: " default-directory nil t))))
  (when (not (file-directory-p directory-name))
    (error "Not a directory: %s" directory-name))
  (if (eql 'nil filter)
      (f-files directory-name)
    (f-files directory-name (lambda (file)
                              (member (f-ext file) filter)))))



(provide 'dionysos-io)
;;; dionysos-io.el ends here
