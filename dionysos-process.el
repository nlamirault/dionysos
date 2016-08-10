;;; dionysos-process.el --- some tools -*- lexical-binding: t; -*-

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

;; required for lexical-let
(require 'cl)


(defun dionysos--process-sentinel (process event hook-fn)
  (when (memq (process-status process)
              '(exit signal))
    (message "[dionysos-process] Process %s event:%s status:%s"
             (process-name process) event (process-exit-status process))
    (when (= 0 (process-exit-status process))
      (message "[dionysos-process] Next: %s" hook-fn)
      (when hook-fn
        (funcall hook-fn)))))

(defun dionysos--create-process (process-name command arguments &optional hook)
  "Create a new asynchronous process.
`PROCESS-NAME' is used to identify this process
`COMMAND' correspond to the program running
`ARGUMENTS' are arguments passed to the program.
`HOOK' is called when process is finished."
  (message "[dionysos-process] Create %s %s %s %s"
           process-name command arguments hook)
  (let ((status (dionysos--status-process process-name)))
    (message "[dionysos-process] Status : %s" status)
    (unless (equal 'run status)
      (lexical-let ((hook hook))
        (let ((process (apply 'start-process process-name nil command arguments)))
          (set-process-sentinel
           process
           (lambda (process event)
             (dionysos--process-sentinel process event hook))))))))

(defun dionysos--kill-process (process-name)
  "Stop a process identified by `PROCESS-NAME'."
  (let ((process (get-process process-name)))
    (when process
      (kill-process process)
      (sleep-for 1))))


(defun dionysos--status-process (process-name)
  "Retrieve the status of the process identified by `PROCESS-NAME'."
  (let ((process (get-process process-name)))
    (when process
      (process-status process))))


(defun dionysos--send-process (process-name message)
  "Send `MESSAGE' to a process identified by `PROCESS-NAME'."
  (let ((process (get-process process-name)))
    (when process
      (process-send-string process-name "pause\n"))))


(provide 'dionysos-process)
;;; dionysos-process.el ends here
