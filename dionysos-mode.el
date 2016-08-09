;;; dionysos-mode.el --- Dionysos mode

;; Copyright (C) 2015-2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either mode 2
;; of the License, or (at your option) any later mode.

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

(defvar dionysos-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in Dionysos buffers.")

(defvar dionysos-mode-hook nil
  "Hook run when entering Dionysos mode.")

(defun dionysos-mode ()
  "Parent major mode for Dionysos buffers."
  (setq buffer-read-only t)
  (setq major-mode 'dionysos-mode)
  (setq mode-name "Dionysos")
  (run-mode-hooks 'dionysos-mode-hook))

(defun dionysos--mode-current-media ()
  "Return the current track at point."
  (get-text-property (point) :dionysos-media))

(defun dionysos--mode-next-media ()
  "Move point to the next track."
  (interactive)
  (let ((pos (next-single-property-change (point) :dionysos-media)))
    (when pos
      (goto-char pos)
      (unless (dionysos--mode-current-media)
        (let ((pos (next-single-property-change pos :dionysos-media)))
          (if pos (goto-char pos)))))))

(defun dionysos--mode-prev-media ()
  "Move point to the next track."
  (interactive)
  (let ((pos (previous-single-property-change (point) :dionysos-media)))
    (when pos
      (goto-char pos)
      (unless (dionysos--mode-current-media)
	(let ((pos (previous-single-property-change pos :dionysos-media)))
	  (if pos (goto-char pos)))))))


(provide 'dionysos-mode)
;;; dionysos-mode.el ends here
