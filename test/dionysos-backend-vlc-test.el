;;; dionysos-backend-vlc-test.el --- Tests for Dionysos VLC backend

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

(ert-deftest test-dionysos-backend-vlc ()
  (with-test-sandbox
   (should (dionysos--get-backend 'vlc))
   (should (custom-variable-p 'dionysos-vlc-command))
   (should (string= "vlc" dionysos-vlc-command))))


(ert-deftest test-dionysos-backend-vlc-start-process ()
  (with-test-sandbox
   (should (eql 'dionysos--vlc-start (dionysos--backend-start 'vlc)))))

(ert-deftest test-dionysos-backend-vlc-stop-process ()
  (with-test-sandbox
   (should (equal 'dionysos--vlc-stop (dionysos--backend-stop 'vlc)))))

(ert-deftest test-dionysos-backend-vlc-play-mp3 ()
  (with-test-sandbox
   (with-music-file
    "resources/Roulement_tambour-01.mp3"
    (dionysos--vlc-start file)
    (should (equal 'run (dionysos--status-process dionysos--process-name)))
    (dionysos--vlc-stop)
    (should (equal nil (dionysos--status-process dionysos--process-name))))))


(provide 'dionysos-backend-vlc-test)
;;; dionysos-backend-vlc-test.el ends here
