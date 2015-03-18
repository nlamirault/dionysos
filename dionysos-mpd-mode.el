;;; dionysos-mpd-mode.el --- Dionysos MPD mode

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


(require 'widget)
(require 'cl-lib)

(require 'f)

(require 'dionysos-custom)
(require 'dionysos-backend-mpd)
(require 'dionysos-volume)

;; Customization

(defgroup dionysos-mpd-mode nil
  "Customization group for `dionysos-mpd-mode'."
  :prefix "dionysos-mpd-mode-"
  :tag "Dionysos MPD Mode"
  :group 'dionysos)

(defcustom dionysos-buffer "*dionysos*"
  "The Dionysos buffer name."
  :type 'string
  :group 'dionysos-mpd-mode)

(defcustom dionysos-padding 2
  "The number of columns used for padding on the left side of the buffer."
  :type 'integer
  :group 'dionysos-mpd-mode)

;; Faces

(defgroup dionysos-mpd-mode-faces '((sx-user custom-group))
  "Customization group for the faces of `dionysos-mpd-mode'."
  :prefix "dionysos-mpd-mode-"
  :tag "Dionysos MPD Mode Faces"
  :group 'dionysos-mpd-mode)

(defface dionysos-song-title
  '((t :weight bold :inherit font-lock-string-name-face))
  "Face used on the song render in the Dionysos buffer."
  :group 'dionysos-mpd-mode-faces)

(defface dionysos-song-artist
  '((t :inherit font-lock-comment-face))
  "Face used on the song render in the Dionysos buffer"
  :group 'dionysos-mpd-mode-faces)

(defface dionysos-song-album
  '((t :inherit font-lock-string-face))
  "Face used on the song render in the Dionysos buffer"
  :group 'dionysos-mpd-mode-faces)

(defface dionysos-song-length
  '((t :weight bold :inherit default))
  "Face used on the song render in the Dionysos buffer"
  :group 'dionysos-mpd-mode-faces)

(defface dionysos-song-track
  '((t :inherit font-lock-warning-face))
  "Face used on the song render in the Dionysos buffer"
  :group 'dionysos-mpd-mode-faces)


;;
;; MPD mode I/O
;;

(defun dionysos--mpd-mode-start ()
  "Start listening music."
  (interactive)
  (dionysos--mpd-start))


(defun dionysos--mpd-mode-stop ()
  "Stop listening music."
  (interactive)
  (dionysos--mpd-stop))

(defun dionysos--mpd-mode-previous ()
  "Listen previous song."
  (interactive)
  (dionysos--mpd-prev))

(defun dionysos--mpd-mode-next ()
  "Listen next song."
  (interactive)
  (dionysos--mpd-next))


;; UI

(defun dionysos--width ()
  "Return the width of the renderable content."
  (- (/ (frame-width) 2) (* dionysos-padding 2)))


(defun dionysos--horizontal-rule ()
  "Insert a horizontal rule into the buffer."
  (widget-insert
   (concat (make-string dionysos-padding ?\s)
	   (make-string (- (dionysos--width) dionysos-padding) ?-)
	   (make-string dionysos-padding ?\s)
	   "\n")))

(defun dionysos--render-row (left right &optional width-right)
  "Render a row with a `LEFT' and a `RIGHT' part.
Optional argument `WIDTH-RIGHT' is the width of the right argument."
  (let* ((width-right (or width-right (length (or right ""))))
	 (width-left (- (dionysos--width)
			(- width-right 1)
			(* 2 dionysos-padding)))
	 (padding (make-string dionysos-padding ?\s)))
    (widget-insert (format
		    (format "%s%%-%s.%ss %%%s.%ss%s\n"
			    padding
			    width-left width-left
			    width-right width-right
			    padding)
		    left right))))


(defun dionysos--render-song (song)
  "Render a `SONG' to the Dionysos MPD buffer."
  ;;(message "Song: %s" song)
  (dionysos--render-row
   (format "%s %s"
           (propertize (number-to-string (dionysos--plist-get song 'Id))
                       'face 'dionysos-song-track)
           (propertize (dionysos--plist-get song 'Title)
                       'face 'dionysos-song-title))
   (propertize (dionysos--plist-get song 'Length)
               'face 'dionysos-song-length))
  (dionysos--render-row
   (format "%s / %s"
           (propertize (dionysos--plist-get song 'Artist)
                       'face 'dionysos-song-artist)
           (propertize (dionysos--plist-get song 'Album)
                       'face 'dionysos-song-album))
   (dionysos--plist-get song 'Filename)))


(defun dionysos--render (songs)
  "Render `SONGS'."
  (let ((start (point)))
    ;;(dionysos--horizontal-rule)
    (cl-loop
     for n from 1 to (length songs)
     do (let ((song (elt songs (- n 1)))
              (start (point)))
          (dionysos--render-song song)
          (put-text-property start (point) :dionysos-media song)))
    (widget-insert "\n")))


;; Mode


(defun dionysos-kill-buffer ()
  "Kill the `dionysos-buffer' and delete the current window."
  (interactive)
  (let ((buffer (get-buffer dionysos-buffer)))
    (when (equal buffer (current-buffer))
      (delete-window))
    (when buffer
      (kill-buffer buffer)))
  (dionysos--mpd-stop))

(defun dionysos-current-media ()
  "Return the current SoundCloud track at point."
  (get-text-property (point) :dionysos-media))


(defun dionysos-next-media ()
  "Move point to the next SoundCloud track."
  (interactive)
  (let ((pos (next-single-property-change (point) :dionysos-media)))
    (when pos
      (goto-char pos)
      (unless (dionysos-current-media)
	(let ((pos (next-single-property-change pos :dionysos-media)))
	  (if pos (goto-char pos)))))))


(defun dionysos-prev-media ()
  "Move point to the next SoundCloud track."
  (interactive)
  (let ((pos (previous-single-property-change (point) :dionysos-media)))
    (when pos
      (goto-char pos)
      (unless (dionysos-current-media)
	(let ((pos (previous-single-property-change pos :dionysos-media)))
	  (if pos (goto-char pos)))))))


(defun dionysos-play-media ()
  "Play current song."
  (interactive)
  (let ((song (dionysos-current-media)))
    (when song
      (dionysos--mpd-start (dionysos--plist-get song 'Id)))))


(defmacro dionysos-with-widget (title &rest body)
  `(progn
     (set-buffer (get-buffer-create dionysos-buffer))
     (switch-to-buffer-other-window dionysos-buffer)
     (kill-all-local-variables)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (remove-overlays)
       (widget-insert (format "\n [%s]\n\n" ,title))
       ,@body)
     (use-local-map widget-keymap)
     (widget-setup)
     (dionysos--mpd-mode)
     (widget-minor-mode)
     (goto-char 0)))

(defvar dionysos--mpd-mode-hook nil)

(defvar dionysos--mpd-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "i") 'dionysos-song-info)
    (define-key map (kbd "p") 'dionysos-prev-media)
    (define-key map (kbd "n") 'dionysos-next-media)
    (define-key map (kbd "c") 'dionysos-play-media)
    (define-key map (kbd "q") 'dionysos-kill-buffer)
    (define-key map (kbd "+") 'dionysos-volume-raise)
    (define-key map (kbd "-") 'dionysos-volume-decrease)
    (define-key map (kbd "s") 'dionysos--mpd-mode-start)
    (define-key map (kbd "SPC") 'dionysos--mpd-mode-stop)
    map)
  "Keymap for `dionysos--mpd-mode' major mode.")

(define-derived-mode dionysos--mpd-mode tabulated-list-mode
  "Dionysos MPD mode"
  "Major mode for Dionysos using MPD."
  :group 'dionysos
  )

;; API

;;;###autoload
(defun dionysos-mpd-playlist ()
  "Show music files from MPD playlist."
  (interactive)
  (dionysos-with-widget
   (propertize "Playlist")
   (dionysos--render (dionysos--mpd-playlist))))


;; ;;;###autoload
;; (defun dionysos-mpd-songs ()
;;   "Show music files from MPD."
;;   (interactive)
;; ;;  (pop-to-buffer "*Dionysos*" nil)
;;   ;;(dionysos--mpd-mode)
;;   (dionysos-with-widget
;;    (propertize "Songs")
;;    (dionysos--render (dionysos--mpd-songs))))


(provide 'dionysos-mpd-mode)
;;; dionysos-mpd-mode.el ends here
