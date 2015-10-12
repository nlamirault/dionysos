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


(defun dionysos--create-process (process-name command arguments &optional hook)
  "Create a new asynchronous process.
`PROCESS-NAME' is used to identify this process
`COMMAND' correspond to the program running
`ARGUMENTS' are arguments passed to the program.
`HOOK' is called when process is finished."
  (message "Create Process : %s %s %s %s"
           process-name command arguments (process-list))
  (let ((status (dionysos--status-process process-name)))
    (message "Process status : %s" status)
    (unless (equal 'run status)
      (let ((process
             (apply 'start-process
                    process-name
                    nil
                    command
                    arguments)))
        ;; (lexical-let
        ;;     ((after-fn hook))
        (set-process-sentinel process
                              (lambda (process event)
                                (when (memq (process-status process)
                                            '(exit signal))
                                  (message "Process End %s %s %s"
                                           (process-name process)
                                           event
                                           (process-exit-status process))
                                  (when (and (= 0 (process-exit-status process))
                                             hook)
                                    (funcall hook)))))))))


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



(provide 'dionysos-process)
;;; dionysos-process.el ends here
