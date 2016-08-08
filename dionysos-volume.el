;;; dionysos-volume.el --- Dionysos volume management

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


(require 'dionysos-process)

;; ------------------
;; Customization
;; ------------------

(defgroup dionysos-volume nil
  "Customization group for `dionysos-volume'."
  :prefix "dionysos-volume-"
  :tag "Dionysos volume management"
  :group 'dionysos)


(defcustom dionysos-volume-cmd 'pamixer
  "Command to control the mixer."
  :group 'dionysos-volume
  :type '(choice
          (const :tag "amixer" amixer)
          (const :tag "pamixer" pamixer)))


;; ------------------
;; Intern
;; ------------------


(defconst dionysos--volume-process "dionysos-volume"
  "The process name for Dionysos volume management process.")

(defconst dionysos--volume-amixer-increase-args
  (list "-q" "sset" "Master" "5%+")
  "Arguments to increase volume for amixer.")

(defconst dionysos--volume-amixer-decrease-args
  (list "-q" "sset" "Master" "5%-")
  "Arguments to decrease volume for amixer.")

(defconst dionysos--volume-pamixer-increase-args
  (list "-i" "5")
  "Arguments to increase volume for pamixer.")

(defconst dionysos--volume-pamixer-decrease-args
  (list "-d" "5")
  "Arguments to decrease volume for pamixer.")


;; ------------------
;; API
;; ------------------


(defun dionysos-volume-raise ()
  "Raise volume."
  (interactive)
  (cond ((eql 'amixer dionysos-volume-cmd)
         (dionysos--create-process dionysos--volume-process
                                   "amixer"
                                   dionysos--volume-amixer-increase-args))
        ((eql 'pamixer dionysos-volume-cmd)
         (dionysos--create-process dionysos--volume-process
                                   "pamixer"
                                   dionysos--volume-pamixer-increase-args))
        (t (message "No volume command available."))))


(defun dionysos-volume-decrease ()
  "Decrease volume."
  (interactive)
  (cond ((eql 'amixer dionysos-volume-cmd)
         (dionysos--create-process dionysos--volume-process
                                   "amixer"
                                   dionysos--volume-amixer-decrease-args))
        ((eql 'pamixer dionysos-volume-cmd)
         (dionysos--create-process dionysos--volume-process
                                   "pamixer"
                                   dionysos--volume-pamixer-decrease-args))
        (t (message "No volume command available."))))


(provide 'dionysos-volume)
;;; dionysos-volume.el ends here
