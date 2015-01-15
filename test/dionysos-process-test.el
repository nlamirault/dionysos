;;; dionysos-process-test.el --- Tests for process information

;; Copyright (C) 2015 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either process 2
;; of the License, or (at your option) any later process.

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

(ert-deftest test-dionysos-process-cant-create-multiple-process ()
  :tags '(current)
  (with-test-sandbox
   (with-music-file
    "resources/France.mp3"
    (dionysos--create-process dionysos--process-name "mpg123" (list file))
    (dionysos--create-process dionysos--process-name "mpg123" (list file))
    (message "Process multiple : %s" (process-list))
    (should (= 1 (length (process-list))))
    (dionysos--kill-process dionysos--process-name)
    (should (equal nil (process-list))))))


(ert-deftest test-dionysos-process-can-start-process ()
  (with-test-sandbox
   (with-music-file
    "resources/Roulement_tambour-01.mp3"
    (dionysos--create-process dionysos--process-name "mpg123" (list file))
    (should (equal 'run (dionysos--status-process dionysos--process-name)))
    (dionysos--kill-process dionysos--process-name)
    (should (equal nil (process-list))))))



(provide 'dionysos-process-test)
;;; dionysos-process-test.el ends here
