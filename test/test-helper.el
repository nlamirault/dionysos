;; test-helper.el --- Test helpers for dionysos.el

;; Copyright (C) 2015 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;; Homepage: https://github.com/nlamirault/dionysos

;;; License:

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ansi)
(require 'cl)
(require 'ert)
(require 'f)
(require 'undercover)

(setq debugger-batch-max-lines (+ 50 max-lisp-eval-depth)
      debug-on-error t)

(defvar username (getenv "HOME"))

(defconst dionysos-testsuite-dir
  (f-parent (f-this-file))
  "The testsuite directory.")

(defconst dionysos-source-dir
  (f-parent dionysos-testsuite-dir)
  "The dionysos.el source directory.")

(defconst dionysos-sandbox-path
  (f-expand "sandbox" dionysos-testsuite-dir)
  "The sandbox path for dionysos.")

(defun cleanup-load-path ()
  "Remove home directory from 'load-path."
  (message (ansi-green "[dionysos] Cleanup path"))
  (mapc #'(lambda (path)
            (when (string-match (s-concat username "/.emacs.d") path)
              (message (ansi-yellow "Suppression path %s" path))
              (setq load-path (delete path load-path))))
        load-path)
  (add-to-list 'load-path default-directory))

(defun load-library (file)
  "Load current library from FILE."
  (let ((path (s-concat dionysos-source-dir file)))
    (message (ansi-yellow "[dionysos] Load library from %s" path))
    (undercover "*.el" (:exclude "*-test.el"))
    (require 'dionysos path)))


(defun setup-dionysos ()
  "Setup Dionysos."
  )

(defmacro with-music-file (filename &rest body)
  `(let ((file (f-join dionysos-testsuite-dir ,filename)))
     ,@body))


(defmacro with-test-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(unwind-protect
       (condition-case nil ;ex
           (let (;;(user-emacs-directory dionysos-sandbox-path)
                 (default-directory dionysos-source-dir))
             ;; (unless (f-dir? dionysos-sandbox-path)
             ;;   (f-mkdir dionysos-sandbox-path))
             (cleanup-load-path)
             (load-library "/dionysos.el")
             (setup-dionysos)
             ,@body)
         ;; (f-delete overseer-sandbox-path :force)))
         )))
         ;; (error
         ;;  (message (ansi-red "[dionysos] Error during unit tests : %s" ex))))))


(provide 'test-helper)
;;; test-helper.el ends here
