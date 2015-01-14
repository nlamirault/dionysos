;;; dionysos-process.el --- some tools

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


(defun dionysos--output-message-sentinel (process msg)
  (when (memq (process-status process)
              '(exit signal))
    (message (concat (process-name process) " - " msg))))

(defun dionysos--create-process (process-name command arguments)
  "Create a new asynchronous process.
`PROCESS-NAME' is used to identify this process
`COMMAND' correspond to the program running
`ARGUMENTS' are arguments passed to the program."
  (let ((process
         (apply 'start-process
                process-name
                nil
                command
                arguments)))
    (set-process-sentinel process 'dionysos--output-message-sentinel)))


(defun dionysos--kill-process (process-name)
  "Stop a process identified by `PROCESS-NAME'."
  (let ((process (get-process process-name)))
    (kill-process process)))


(defun dionysos--status-process (process-name)
  "Retrieve the status of the process identified by `PROCESS-NAME'."
  (let ((process (get-process process-name)))
    (when process
      (process-status process))))



(provide 'dionysos-process)
;;; dionysos-process.el ends here
