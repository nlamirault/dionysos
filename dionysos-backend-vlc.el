;;; dionysos-backend-vlc.el --- Dionysos VLC backend

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

(require 'dionysos-backend)
(require 'dionysos-notify)
(require 'dionysos-process)

(dionysos--define-backend vlc
  :name "VLC"
  :command "vlc"
  :filter '("ogg" "mp3" "wav" "flac")
  :start 'dionysos--vlc-start
  :pause 'dionysos--vlc-pause
  :stop 'dionysos--vlc-stop)


(defun dionysos--vlc-start (filename &optional hook)
  "Start playing music.
`FILENAME' using VLC.
`HOOK' is for next action."
  (message "[dionysos-vlc] Start %s Next %s" filename hook)
  (dionysos--notify
   (format "%s\n" (file-name-base filename))'info)
  (dionysos--create-process dionysos--process-name
                            dionysos-vlc-command
                            (append '("--intf" "rc") (list filename) '("vlc://quit"))
                            hook))


(defun dionysos--vlc-stop ()
  "Stop VLC process."
  (message "[dionysos-vlc] Stop")
  (dionysos--kill-process dionysos--process-name))


(defun dionysos--vlc-pause ()
  "Pause VLC process."
  (message "[dionysos-vlc] Pause")
  (dionysos--send-process dionysos--process-name "pause"))


(defun dionysos--vlc-seek-to (val)
  "Send a seek to command to VLC process."
  (message "[dionysos-vlc] Seek to %s" val)
  (dionysos--send-process dionysos--process-name (format "seek %d\n" val)))



(provide 'dionysos-backend-vlc)
;;; dionysos-backend-vlc.el ends here
