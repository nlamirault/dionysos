;;; dionysos-volume-test.el --- Unit tests for Dionysos volume

;; Copyright (C) 2015-2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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


(ert-deftest test-dionysos-volume-amixer-command ()
  :tags '(volume)
  (with-test-sandbox
   (let* ((dionysos-volume-cmd 'amixer)
          (mixer (dionysos--volume-mixer)))
     (should (string-equal dionysos--volume-amixer (car mixer)))
     (should (eq dionysos--volume-amixer-increase-args (cadr mixer)))
     (should (eq dionysos--volume-amixer-decrease-args (caddr mixer))))))


(defun dionysos--check-volume-process ()
  (message "Process multiple : %s" (process-list))
  (should (= 1 (length (process-list))))
  (dionysos--kill-process dionysos--volume-process)
  (should (equal nil (process-list))))


(ert-deftest test-dionysos-volume-raise ()
  :tags '(volume)
  (cond ((executable-find "pamixer")
         (setq dionysos-volume-cmd 'pamixer))
        ((executable-find "pamixer")
         (setq dionysos-volume-cmd 't)))
  (when dionysos-volume-cmd
    (with-test-sandbox
     (with-music-file
      "resources/France.mp3"
      (dionysos-volume-raise)
      (dionysos--check-volume-process)))))


(ert-deftest test-dionysos-volume-pamixer-command ()
  :tags '(volume)
  (with-test-sandbox
   (let* ((dionysos-volume-cmd 'pamixer)
          (mixer (dionysos--volume-mixer)))
     (should (string-equal dionysos--volume-pamixer (car mixer)))
     (should (eq dionysos--volume-pamixer-increase-args (cadr mixer)))
     (should (eq dionysos--volume-pamixer-decrease-args (caddr mixer))))))


(ert-deftest test-dionysos-volume-decrease ()
  :tags '(volume current)
  (cond ((executable-find "pamixer")
         (setq dionysos-volume-cmd 'pamixer))
        ((executable-find "pamixer")
         (setq dionysos-volume-cmd 't)))
  (when dionysos-volume-cmd
    (with-test-sandbox
     (with-music-file
      "resources/France.mp3"
      (dionysos-volume-decrease)
      (dionysos--check-volume-process)))))



(provide 'dionysos-volume-test)
;;; dionysos-volume-test.el ends here
