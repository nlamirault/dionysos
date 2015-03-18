;;; dionysos-backend-mpd-test.el --- Tests for Dionysos MPD backend

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

(ert-deftest test-dionysos-backend-mpd ()
  (with-test-sandbox
   (should (dionysos--get-backend 'mpd))
   (should (string-equal "localhost" dionysos-mpd-host))
   (should (= 6600 dionysos-mpd-port))))


(provide 'dionysos-backend-mpd-test)
;;; dionysos-backend-mpd-test.el ends here
