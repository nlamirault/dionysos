;;; dionysos-ui.el --- Dionysos UI tools

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


;; to display faces in IELM :
;; ELISP (list-faces-display "dionysos")
;;

;; Faces

(defface dionysos--title
  '((((class color) (background light)) :foreground "red" :weight semi-bold)
    (((class color) (background dark)) :foreground "green" :weight semi-bold))
  "face of Dionysos information"
  :group 'dionysos)

(defface dionysos--gray-face
  '((((class color)) :foreground "#b1b6b6"))
  "Gray color."
  :group 'dionysos)

(defface dionysos--cyan-face
  '((((class color)) :foreground "#00ffff"))
  "Cyan color."
  :group 'dionysos)

(defface dionysos--yellow-face
  '((((class color)) :foreground "#e5e500"))
  "Yellow color."
  :group 'dionysos)

(defface dionysos--orange-face
  '((((class color)) :foreground "#ff5500"))
  "Orange color."
  :group 'dionysos)

(defface dionysos--red-face
  '((((class color)) :foreground "#cd4d40"))
  "Red color."
  :group 'dionysos)

(defface dionysos--green-face
  '((((class color)) :foreground "#61b361"))
  "Green color."
  :group 'dionysos)


(defun colorize-term (term color)
  "Colorize `TERM' using `COLOR'."
  (cond
   ((eql color 'red)
    (propertize term 'face 'dionysos--red-face))
   ((eql color 'green)
    (propertize term 'face 'dionysos--green-face))
   (t term)))


;; (defun colorize-dot (color)
;;   (cond
;;    ((string= color  "red")
;;     (propertize "●" 'face 'dionysos--red-face))
;;    ((string= color "yellow")
;;     (propertize "●" 'face 'dionysos--yellow-face))
;;    ((string= color  "green")
;;     (propertize "●" 'face 'dionysos--green-face))
;;    (t (concat "Unknown: " "'" color "' "))))

(provide 'dionysos-ui)
;;; dionysos-ui.el ends here
