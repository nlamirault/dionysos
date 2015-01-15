;;; dionysos-backend-vlc.el --- Dionysos VLC backend

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

(require 'dionysos-backend)
(require 'dionysos-process)

(dionysos--define-backend vlc
  :name "VLC"
  :command "vlc"
  :filter '("ogg" "mp3" "wav" "flac")
  :start 'dionysos--vlc-start
  :stop 'dionysos--vlc-stop)


(defun dionysos--vlc-start (filename &optional hook)
  "Start playing music.
`FILENAME' using VLC.
`HOOK' is for next action."
  (message "Create process %s" filename)
  (dionysos--create-process dionysos--process-name
                            dionysos-vlc-command
                            (append '("--intf" "rc") (list filename) '("vlc://quit"))
                            hook))


(defun dionysos--vlc-stop ()
  "Stop VLC process."
  (dionysos--kill-process dionysos--process-name))

(provide 'dionysos-backend-vlc)
;;; dionysos-backend-vlc.el ends here
