;;; dionysos-backend-mpd.el --- Dionysos MPD backend

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

(require 'libmpdee)

(require 'dionysos-backend)
(require 'dionysos-custom)
(require 'dionysos-notify)
(require 'dionysos-process)

(dionysos--define-backend mpd
  :name "MPD"
  :command nil
  :filter '("ogg" "mp3" "wav" "flac")
  :start 'dionysos--mpd-start
  :stop 'dionysos--mpd-stop)


;; Customization

(defcustom dionysos-mpd-host "localhost"
  "Host running mpd."
  :type 'string
  :group 'dionysos-mpd)

(defcustom dionysos-mpd-port 6600
  "Port to connect to."
  :type 'integer
  :group 'dionysos-mpd)

(defvar dionysos--mpd-con nil
  "The MPD connection.")


;; Core

(defmacro with-mpd (&rest body)
  `(progn
     (unless (equal 'ready dionysos--mpd-con)
       (dionysos--mpd-connect))
     ,@body))

(defun dionysos--mpd-connect ()
  (setq dionysos--mpd-con (mpd-conn-new dionysos-mpd-host dionysos-mpd-port)))


(defun dionysos--mpd-status ()

  (with-mpd
   (mpd-get-status dionysos--mpd-con)))


(defun dionysos--mpd-start (&optional song-id)
  "Start MPD playing music.
`SONG-ID' is to specify a song."
  (with-mpd
   (dionysos--mpd-connect)
   (if song-id
       (mpd-play dionysos--mpd-con song-id t)
     (mpd-play dionysos--mpd-con))
   (let ((song (mpd-get-current-song dionysos--mpd-con)))
     (dionysos--notify
      (format "%s\n%s"
              (dionysos--plist-get song 'Title)
              (dionysos--plist-get song 'Artist))
      'info))))


(defun dionysos--mpd-stop ()
  "Stop MPD playing song."
  (with-mpd
   (mpd-stop dionysos--mpd-con)))


(defun dionysos--mpd-prev ()
  "Read previous song."
  (with-mpd
   (mpd-prev dionysos--mpd-con)))


(defun dionysos--mpd-next ()
  "Read next song."
  (with-mpd
   (mpd-next dionysos--mpd-con)))


(defun dionysos--mpd-songs ()
  "Retrieve all songs from MPD."
  (with-mpd
   ;;(mpd-get-directory-songs dionysos--mpd-con)))
   (mpd-get-songs dionysos--mpd-con "listallinfo")))


(defun dionysos--mpd-playlist ()
  "Return the MPD playlist."
  (with-mpd
   ;;(mpd-get-playlist dionysos--mpd-con)))
   (mpd-get-songs dionysos--mpd-con "playlistinfo")))


(defun dionysos--mpd-current-song ()
  "Return the current song."
  (with-mpd
   (mpd-get-current-song dionysos--mpd-con)))


(defun dionysos--plist-get (plist prop)
  "Extract a value from the property list or return an empty string.
`PLIST' is the property list
`PROP' is the key"
  (let ((data (plist-get plist prop)))
    (if data
        data
      "")))


(provide 'dionysos-backend-mpd)
;;; dionysos-backend-mpd.el ends here
