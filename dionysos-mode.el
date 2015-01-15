;;; dionysos-mode.el --- Dionysos mode

;; Copyright (C) 2015 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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


(provide 'dionysos-mode)
;;; dionysos-mode.el ends here
