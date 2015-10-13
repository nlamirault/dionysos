;;; dionysos.el --- Dionysos, a music player for Emacs

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;; URL: https://github.com/nlamirault/dionysos
;; Version: 0.4.0
;; Keywords: music

;; Package-Requires: ((libmpdee "2.1.0") (alert "1.2") (s "1.9.0") (dash "2.9.0") (pkg-info "0.5.0") (cl-lib "0.5"))

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

(require 'cl-lib)

(require 'dionysos-custom)
(require 'dionysos-version)
(require 'dionysos-io)
(require 'dionysos-notify)
(require 'dionysos-process)
(require 'dionysos-ui)
(require 'dionysos-backend)
(require 'dionysos-backend-vlc)
(require 'dionysos-backend-mplayer)
(require 'dionysos-backend-mpd)
(require 'dionysos-volume)
(require 'dionysos-mode)
(require 'dionysos-directory-mode)
(require 'dionysos-files-mode)
(require 'dionysos-mpd-mode)

(provide 'dionysos)
;;; dionysos.el ends here
