;;; dionysos-fs-mode.el --- Dionysos Filesystem mode

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


(require 'widget)
(require 'cl-lib)

(require 'f)

(require 'dionysos-io)
(require 'dionysos-custom)
(require 'dionysos-backend-vlc)
(require 'dionysos-mode)
(require 'dionysos-volume)

;; ------------------
;; Customization
;; ------------------

(defgroup dionysos-fs-mode nil
  "Customization group for `dionysos-fs-mode'."
  :prefix "dionysos-fs-mode-"
  :tag "Dionysos Filesystem Mode"
  :group 'dionysos)

(defcustom dionysos-fs-mode-buffer "*dionysos-fs*"
  "The Dionysos buffer name."
  :type 'string
  :group 'dionysos-fs-mode)

(defcustom dionysos-fs-mode-padding 2
  "The number of columns used for padding on the left side of the buffer."
  :type 'integer
  :group 'dionysos-fs-mode)

;; ;; ------------------
;; ;; Faces
;; ;; ------------------

(defgroup dionysos-fs-mode-faces '((dionysos-fs-mode custom-group))
  "Customization group for the faces of `dionysos-fs-mode'."
  :prefix "dionysos-fs-mode-"
  :tag "Dionysos filesystem-mode faces"
  :group 'dionysos-fs-mode)

(defface dionysos-fs-mode-song-file
  '((t :weight bold :inherit font-lock-warning-name-face))
  "Face used on the song render in the Dionysos buffer."
  :group 'dionysos-fs-mode-faces)

(defface dionysos-fs-mode-song-title
  '((t :weight bold :inherit font-lock-warning-name-face))
  "Face used on the song render in the Dionysos buffer."
  :group 'dionysos-fs-mode-faces)

(defface dionysos-fs-mode-song-artist
  '((t :inherit font-lock-comment-face))
  "Face used on the song render in the Dionysos buffer"
  :group 'dionysos-fs-mode-faces)

(defface dionysos-fs-mode-song-album
  '((t :inherit font-lock-string-face))
  "Face used on the song render in the Dionysos buffer"
  :group 'dionysos-fs-mode-faces)

(defface dionysos-fs-mode-song-track
  '((t :inherit font-lock-function-name-face))
  "Face used on the song render in the Dionysos buffer"
  :group 'dionysos-fs-mode-faces)

(defface dionysos-fs-mode-song-type
  '((t :inherit font-lock-comment-face))
  "Face used on the song render in the Dionysos buffer"
  :group 'dionysos-fs-mode-faces)

;; ;; ------------------
;; ;; Backend I/O
;; ;; ------------------

(defun dionysos--fs-mode-start ()
  "Start playing song."
  (interactive)
  (let ((song (dionysos--mode-current-media)))
    (if song
        (dionysos--with-backend
         (funcall (dionysos--backend-start dionysos-backend)
                  (s-trim song)
                  'dionysos--fs-mode-next-action))
      (message "[dionysos-fs] No song available"))))

(defun dionysos--fs-mode-stop ()
  "Stop playing song."
  (interactive)
  (dionysos--with-backend
   (funcall (dionysos--backend-stop dionysos-backend))))

(defun dionysos--fs-mode-next ()
  "Play next song."
  (interactive)
  (dionysos--with-backend
   (dionysos--mode-next-media)
   (dionysos--fs-mode-stop)
   (dionysos--fs-mode-start)))

(defun dionysos--fs-mode-previous ()
  "Play previous song."
  (interactive)
  (dionysos--with-backend
   (dionysos--mode-prev-media)
   (dionysos--fs-mode-stop)
   (dionysos--fs-mode-start)))

(defun dionysos--fs-mode-pause ()
  "Pause playing song."
  (interactive)
  (dionysos--with-backend
   (funcall (dionysos--backend-pause dionysos-backend))))

(defun dionysos--fs-mode-quit ()
  "Stop player and exit."
  (interactive)
  (dionysos--with-backend
   (dionysos--fs-mode-stop))
  (kill-buffer dionysos-fs-mode-buffer))

(defun dionysos--fs-mode-next-action ()
  "Next action after process end."
  (dionysos--fs-mode-next)
  (dionysos--fs-mode-start))


;; ;; ------------------
;; ;; UI
;; ;; ------------------


(defun dionysos--fs-mode-width ()
  "Return the width of the renderable content."
  (- (/ (frame-width) 2) (* dionysos-fs-mode-padding 2)))


(defun dionysos--fs-mode-horizontal-rule ()
  "Insert a horizontal rule into the buffer."
  (widget-insert
   (concat (make-string dionysos-fs-mode-padding ?\s)
	   (make-string (- (dionysos--fs-mode-width) dionysos-fs-mode-padding) ?-)
	   (make-string dionysos-fs-mode-padding ?\s)
	   "\n")))

(defun dionysos--fs-mode-render-row (left right &optional width-right)
  "Render a row with a `LEFT' and a `RIGHT' part.
Optional argument `WIDTH-RIGHT' is the width of the right argument."
  (widget-insert (format "[%s] %s\n" right left)))

(defun dionysos--fs-mode-render-multilines-row (left right left-2 right-2 &optional width-right)
  "Render a row with a `LEFT' and a `RIGHT' part.
Optional argument `WIDTH-RIGHT' is the width of the right argument."
  (widget-insert (format "> %s - %s\n%s / %s\n" left right left-2 right-2)))

(defun dionysos--fs-mode-render-song (song)
  "Render a `SONG' to the Dionysos buffer."
  (message "Song: %s" song)
  (if (executable-find "id3")
      (let* ((tags (dionysos--id3-tag-info song))
             (track (gethash "Track" tags))
             (title (gethash "Title" tags))
             (artist (gethash "Artist" tags))
             (album (gethash "Album" tags)))
        (dionysos--fs-mode-render-multilines-row
         (if track
             (format "%s" (propertize track 'face 'dionysos-fs-mode-song-track))
           "")
         (if title
             (format "%s" (propertize title 'face 'dionysos-fs-mode-song-title))
           (format "%s" (propertize (file-name-base song) 'face 'dionysos-fs-mode-song-type)))
         (if artist
             (format "%s" (propertize artist 'face 'dionysos-fs-mode-song-artist))
           "")
         (if album
             (format "%s" (propertize album 'face 'dionysos-fs-mode-song-album))
           "")))
    (dionysos--fs-mode-render-row
     (format "%s" (propertize  (file-name-base song) 'face 'dionysos-fs-mode-song-file))
     (format "%s" (propertize (file-name-extension song) 'face 'dionysos-fs-mode-song-type)))))


(defun dionysos--fs-mode-render (songs)
  "Render `SONGS'."
  (let ((start (point)))
    ;;(dionysos--horizontal-rule)
    (cl-loop
     for n from 1 to (length songs)
     do (let ((song (elt songs (- n 1)))
              (start (point)))
          (dionysos--fs-mode-render-song song)
          (put-text-property start (point) :dionysos-media song)))
    (widget-insert "\n")))



;; ;; ------------------
;; ;; Mode
;; ;; ------------------

(defmacro dionysos--fs-mode-with-widget (title &rest body)
  `(progn
     (set-buffer (get-buffer-create dionysos-fs-mode-buffer))
     (switch-to-buffer-other-window dionysos-fs-mode-buffer)
     (kill-all-local-variables)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (remove-overlays)
       (widget-insert (format "\n[%s]\n\n" ,title))
       ,@body)
     (use-local-map widget-keymap)
     (widget-setup)
     (dionysos--fs-mode)
     (widget-minor-mode)
     (goto-char 0)))

(defvar dionysos--fs-mode-hook nil)

(defvar dionysos--fs-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "p") 'dionysos--fs-mode-previous)
    (define-key map (kbd "n") 'dionysos--fs-mode-next)
    (define-key map (kbd "s") 'dionysos--fs-mode-start)
    (define-key map (kbd "SPC") 'dionysos--fs-mode-stop)
    (define-key map (kbd "P") 'dionysos--fs-mode-pause)
    (define-key map (kbd "q") 'dionysos--fs-mode-quit)
    (define-key map (kbd "+") 'dionysos-volume-raise)
    (define-key map (kbd "-") 'dionysos-volume-decrease)


    map)
  "Keymap for `dionysos--fs-mode' major mode.")

(define-derived-mode dionysos--fs-mode tabulated-list-mode
  "Dionysos Filesysteme mode"
  "Major mode for Dionysos."
  :group 'dionysos
  )


;; ;; ------------------
;; ;; API
;; ;; ------------------

(defvar dionysos--fs-mode-history nil)


;;;###autoload
(defun dionysos-fs-list (directory)
  "Show music files in `DIRECTORY'."
  (interactive
   (list (read-from-minibuffer "Music directory : "
                               (car dionysos--fs-mode-history)
                               nil
                               nil
                               'dionysos--fs-mode-history)))
  (dionysos--fs-mode-with-widget
   (propertize "Playlist")
   (dionysos--fs-mode-render
    (dionysos--list-directory directory '("ogg" "mp3" "wav" "flac")))))


(provide 'dionysos-fs-mode)
;;; dionysos-fs-mode.el ends here
