;;; dionysos-backend-test.el --- Tests for Dionysos backend

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

(ert-deftest test-dionysos-backend-default-player ()
  (with-test-sandbox
   (should (equal nil dionysos-backend))))

(ert-deftest test-dionysos-backend-foobar ()
  (with-test-sandbox
   (dionysos--define-backend foo
     :name "foo"
     :command "/usr/local/bin/foo"
     :filter '("ogg" "mp3")
     :start 'dyonisis--foo-start-player)
   (should (dionysos--get-backend 'foo))
   ;;(should (equal '(vlc mplayer foo) dionysos-backends))
   (should (custom-variable-p 'dionysos-foo-command))))


(provide 'dionysos-backend-test)
;;; dionysos-backend-test.el ends here
