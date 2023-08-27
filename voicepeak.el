;;; voicepeak.el --- VOICEPEAK CLI binding           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 01 Apr 2023
;; Version: 0.0.1
;; Keywords: multimedia
;; URL: https://github.com/zonuexe/voicepeak.el
;; Package-Requires: ((emacs "28.1") (compat "29") (async "1.9") (mpv "0.2"))

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; VOICEPEAK is a character reading software.

;;; Code:
(require 'async)
(require 'mpv)

(defgroup voicepeak nil
  "VOICEPEAK is a character reading software."
  :group 'multimedia)

(eval-and-compile
  (defconst voicepeak-system-path
    (cl-case system-type
      (darwin "/Applications/voicepeak.app/Contents/MacOS/voicepeak")
      (windows-nt "C:\\Program Files\\VOICEPEAK\voicepeak.exe"))))

(defcustom voicepeak-executable (or (executable-find "voicepeak")
                           (executable-find voicepeak-system-path))
  "VOICEPEAK executable."
  :type 'string
  :safe #'stringp
  :group 'voicepeak)

(defcustom voicepeak-narrator nil
  "VOICEPEAK narrator."
  :type '(choice string nil)
  :safe (lambda (v) (or (stringp v) (null v)))
  :group 'voicepeak)

(defvar voicepeak--running-process nil)
(defvar voicepeak--temp-files '())
(defvar voicepeak--queue '())

(defun voicepeak-list-narrator ()
  "Return a list of VOICEPEAK narrators."
  (let* ((args (list voicepeak-executable "--list-narrator" "2>/dev/null"))
         (cmd (mapconcat #'shell-quote-argument args " "))
         output result)
    (setq output
          (with-temp-buffer
            (with-output-to-string
              (with-current-buffer standard-output
                (shell-command cmd (current-buffer))))))
    (cl-loop for line in (split-string output "\n")
             unless (string-prefix-p "iconv_open" line)
             collect line)))

(defun voicepeak--dispatch ()
  "Dispatch VOICEPEAK asynchronous process."
  (message "[voicepeak] called voicepeak-dispatch, running-process: %s, queue: %s"
           voicepeak--running-process voicepeak--queue)
  (unless (and voicepeak--running-process (null voicepeak--queue))
    (if-let* ((queue (pop voicepeak--queue))
              (temp-file (plist-get queue :temp)))
        (progn
          (setq voicepeak--running-process t)
          (apply
           #'async-start-process
           "voicepeak"
           voicepeak-executable
           (lambda (result)
             (setq voicepeak--running-process nil)
             (mpv-play temp-file)
             (run-with-timer 10 nil #'voicepeak--dispatch))
           (voicepeak--build-cmd queue)))
      (user-error "[voicepeak] Error dispatch: queue %s" voicepeak--queue))))

(defun voicepeak--build-cmd (plist)
  "Build the VOICEPEAK command line arguments by PLIST."
  (append (when-let (narrator (plist-get plist :narrator)) (list  "-n" narrator))
          (list "-s" (plist-get plist :message)
                "-o" (plist-get plist :temp))))

(cl-defun voicepeak--enqueue (message temp-file &key narrator)
  "Enqueue MESSAGE, NARRATOR and TEMP-FILE for dispatch VOICEPEAK."
  (setq voicepeak--queue (nconc voicepeak--queue
                       (list (list :narrator narrator :message message :temp temp-file))))
  (message "[voicepeak] enqueued %s" voicepeak--queue))

;;;###autoload
(defun voicepeak-say (message)
  "Speak MESSAGE by VOICEPEAK."
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-string "Input message: "))))
  (let ((temp (voicepeak--create-temp message voicepeak-narrator)))
    (voicepeak--enqueue message temp :narrator voicepeak-narrator)
    (voicepeak--dispatch)))

(defun voicepeak--create-temp (message narrator)
  "Create new temp file for VOICEPEAK by MESSAGE and NARRATOR."
  (let* ((prefix (format "voicepeak-%s-%s" (or narrator "default") (md5 message)))
         (temp (make-temp-file prefix nil ".wav"))
         (key (list :message message :narrator narrator)))
    (prog1 temp
      (add-to-list 'voicepeak--temp-files (cons key temp)))))

(provide 'voicepeak)
;;; voicepeak.el ends here
