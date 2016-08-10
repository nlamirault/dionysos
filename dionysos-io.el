;;; dionysos-io.el --- Dionysos input/output tools

;; Copyright (C) 2015-2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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
(require 's)


(defun dionysos--list-directory (directory-name &optional filter)
  "Insert a new directory `DIRECTORY-NAME' into the dionysos buffer.
`FILTER' is used to remove some files."
  (interactive (list (expand-file-name
                      (read-directory-name
                       "Insert directory: " default-directory nil t))))
  (when (not (file-directory-p directory-name))
    (error "Not a directory: %s" directory-name))
  (if (eql 'nil filter)
      (f-files directory-name nil t)
    (f-files directory-name
             (lambda (file)
               (member (f-ext file) filter))
             t)))


(defun dionysos--id3-tag-info (filename)
  "Extract ID3 tags from MP3 file using `FILENAME' into an hashtable.."
  (let ((tags (make-hash-table   :test 'equal)))
    (mapc (lambda (kv)
            (let ((data (split-string kv ":")))
              (puthash (first data) (s-trim (second data)) tags)))
          (split-string
           (shell-command-to-string (s-concat "id3 " filename)) "\n" t))
    tags))



(provide 'dionysos-io)
;;; dionysos-io.el ends here
