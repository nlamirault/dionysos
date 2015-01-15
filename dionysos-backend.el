;;; dionysos-backend.el --- Dionysos music backend

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


(defvar dionysos-backends '()
  "List of available music backends.")

(defvar dionysos-player nil
  "The currently music player.")

(defconst dionysos--process-name "dionysos"
  "Name of the Dionysos music player process.")

(make-variable-buffer-local 'dionysos-player)

(defmacro dionysos--define-backend (name &rest options)
  "Macro which define a new music backend.
`NAME' is for display
`OPTIONS' specify backend arguements."
  (let* ((group-backend-name
          (intern (format "dionysos-%s" name)))
         (command-name (plist-get options :command))
         (command (eval command-name))
         (command-name-variable
          (intern (format "dionysos-%s-command" name))))
    `(progn
       (defgroup ,group-backend-name nil
         ,(format "The %s Dionysos backend." name)
         :prefix ,(format "dionysos-%s-" name)
         :group 'dionysos)
       (defcustom ,command-name-variable ,command
         ,(format "The name of the `%s' executable." name)
         :type 'string
         :group ',group-backend-name)
       (add-to-list 'dionysos-backends ',name t))))

(provide 'dionysos-backend)
;;; dionysos-backend.el ends here
