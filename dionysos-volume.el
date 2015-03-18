;;; dionysos-volume.el --- Dionysos volume management

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


(require 'dionysos-process)


(defconst dionysos--volume-process "dionysos-volume"
  "The process name for Dionysos volume management process.")


(defun dionysos-volume-raise ()
  "Raise volume."
  (interactive)
  (dionysos--create-process dionysos--volume-process
                            "amixer"
                            (list "-q" "sset" "Master" "5%+")))


(defun dionysos-volume-decrease ()
  "Decrease volume."
  (interactive)
  (dionysos--create-process dionysos--volume-process
                            "amixer"
                            (list "-q" "sset" "Master" "5%-")))



(provide 'dionysos-volume)
;;; dionysos-volume.el ends here
