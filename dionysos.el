;;; dionysos.el --- Dionysos, a music player for Emacs

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;; URL: https://github.com/nlamirault/dionysos
;; Version: 0.1.0
;; Keywords: music

;; Package-Requires: ((s "1.9.0") (dash "2.9.0") (pkg-info "0.5.0"))

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

;; Provides a music player for Emacs.

;;; Installation:

;; Available as a package in melpa.milkbox.net.

;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;
;; M-x package-install dionysos

;;; Usage:

;;; Code:

;; Customization

(defgroup dionysos nil
  "A music player for Emacs."
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/nlamirault/dionysos")
  :link '(emacs-commentary-link :tag "Commentary" "dionysos"))

;; Dionysos dependencies

(require 'dionysos-version)
(require 'dionysos-ui)
(require 'dionysos-backend)
(require 'dionysos-backend-vlc)

(provide 'dionysos)
;;; dionysos.el ends here
