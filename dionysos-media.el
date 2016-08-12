;;; dionysos-media.el --- Dionysos media tools

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


(defun dionysos--find-project-path ()
  (let ((dir))
     (mapc (lambda (path)
                 (when (s-contains-p "dionysos" path)
                   (setq dir path)))
               load-path)
     dir))


(defun dionysos--find-image (file-name)
  "Find an image specified by `FILE-NAME'."
  (let ((image-load-path
         (cons (expand-file-name "images" (dionysos--find-project-path))
               (and (boundp 'image-load-path)
                    image-load-path))))
    (find-image
     (list (list :ascent 'center
                 :file file-name
                 :type (image-type-from-file-name file-name)
                 :foreground (face-foreground 'default nil 'default)
                 :background (face-background 'default nil 'default))))))



(defun dionysos--get-logo ()
  "Retrieve the Dionysos logo image."
  (dionysos--find-image "dionysos.png"))



(provide 'dionysos-media)
;;; dionysos-media.el ends here
