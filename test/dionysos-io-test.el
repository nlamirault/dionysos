;;; dionysos-io-test.el --- Tests for io information

;; Copyright (C) 2015 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either io 2
;; of the License, or (at your option) any later io.

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


(ert-deftest test-dionysos-list-directory ()
  (with-test-sandbox
   (let ((files (dionysos--list-directory
                 (f-join dionysos-testsuite-dir "resources"))))
     (should (= 5 (length files))))))

(ert-deftest test-dionysos-list-directory-simple-filter ()
  (with-test-sandbox
   (let ((files (dionysos--list-directory
                 (f-join dionysos-testsuite-dir "resources")
                 '("mp3"))))
     (should (= 3 (length files))))))

(ert-deftest test-dionysos-list-directory-multiple-filters ()
  (with-test-sandbox
   (let ((files (dionysos--list-directory
                 (f-join dionysos-testsuite-dir "resources")
                 '("mp3" "ogg"))))
     (should (= 4 (length files))))))


(provide 'dionysos-io-test)
;;; dionysos-io-test.el ends here
