;;; dionysos-io-test.el --- Tests for io information

;; Copyright (C) 2015-2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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
  :tags '(io)
  (with-test-sandbox
   (let ((files (dionysos--list-directory
                 (f-join dionysos-testsuite-dir "resources"))))
     (should (= 5 (length files))))))


(ert-deftest test-dionysos-list-directory-simple-filter ()
  :tags '(io)
  (with-test-sandbox
   (let ((files (dionysos--list-directory
                 (f-join dionysos-testsuite-dir "resources")
                 '("mp3"))))
     (should (= 3 (length files))))))


(ert-deftest test-dionysos-list-directory-multiple-filters ()
  :tags '(io)
  (with-test-sandbox
   (let ((files (dionysos--list-directory
                 (f-join dionysos-testsuite-dir "resources")
                 '("mp3" "ogg"))))
     (should (= 4 (length files))))))


(ert-deftest test-dionysos-list-directory-with-file ()
  :tags '(io)
  (with-test-sandbox
   (should-error (dionysos--list-directory
                  (f-join dionysos-testsuite-dir "dionysos-io-test.el")))))


(ert-deftest test-dionysos-extract-id3-tags ()
  :tags '(io)
  (with-test-sandbox
   (with-music-file-2
    file "resources/France.mp3"
    (let ((tags (dionysos--id3-tag-info file)))
      (should (string-equal "ID3v2.3" (gethash "Metadata" tags)))
      (should (string-equal "hymne france - france anthem" (gethash "Title" tags)))
      (should (string-equal "http" (gethash "Artist" tags)))
      (should (string-equal "2008" (gethash "Year" tags)))
      (should (string-equal "Other" (gethash "Genre" tags)))))))


(provide 'dionysos-io-test)
;;; dionysos-io-test.el ends here
