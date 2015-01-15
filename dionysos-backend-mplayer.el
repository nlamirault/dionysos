;;; dionysos-backend-mplayer.el --- Dionysos MPlayer backend

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

(dionysos--define-backend mplayer
  :name "MPlayer"
  :command "mplayer"
  :filter '("ogg" "mp3" "wav" "flac")
  :start 'dionysos--mplayer-start
  :stop 'dionysos--mplayer-stop)


(defun dionysos--mplayer-start (filename &optional arguments)
  (dionysos--create-process dionysos--process-name
                            dionysos-mplayer-command
                            (append '("-quiet" "-really-quiet") (list filename))))

(defun dionysos--mplayer-stop ()
  (dionysos--kill-process dionysos--process-name))

(provide 'dionysos-backend-mplayer)
;;; dionysos-backend-mplayer.el ends here
