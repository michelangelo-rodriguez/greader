;;; greader-speechd.el --- speech-dispatcher back-end for greader  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

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

;;; Code:

;;; customization variables
(defgroup greader-speechd
  nil
  "speech-dispatcher back-end for greader"
  :group 'greader)

(defcustom greader-speechd-executable "spd-say"
  "Executable file name."
  :tag "speech-dispatcher client executable file name"
  :type 'string)

(defcustom greader-speechd-language "en"
  "Language of speech-dispatcher client to speak in."
  :tag "speech-dispatcher language"
  :type 'string)

(defcustom greader-speechd-rate 10
  "Rate of speech.
Can be a value between -100 and 100."
  :tag "speech-dispatcher rate"
  :type 'integer)

(defcustom greader-speechd-punctuation "none"
  "Punctuation level of speech-dispatcher client to speak.
It must be one of the following:
none, some, or all."
  :tag "speech-dispatcher punctuation level"
  :type 'string)

;;; code
(defun greader-speechd--find-executable ()
  "tries to find speech-dispatcher client using greader-speechd-executable as basename."
  (locate-file greader-speechd-executable exec-path))

(defun greader-speechd-set-language
    (&optional lang)
  "sets language 'lang' for speech-dispatcher client.
if lang is omitted, it looks in variable greader-speechd-language and retrieves the appropriate string used by spd-say or another client compatible."
  (if (not lang)
      (concat "-l" greader-speechd-language)
    (progn
      (setq-local greader-speechd-language lang)
      (concat "-l" lang))))

(defun greader-speechd-set-rate
    (&optional rate)
  "returns parameter suitable for spd-say to set speech rate.
for further documentation, see the documentation for greader-speechd-rate variable."
  (if (not rate)
      (concat "-r " (number-to-string greader-speechd-rate))
    (progn
      (setq-local greader-speechd-rate rate)
      (concat "-r " (number-to-string rate)))))

(defun greader-speechd-set-punctuation (&optional punct)
  "returns a suitable parameter to pass to spd-say for setting punctuation leve.
punct must be a numeric value, 0 for no punctuation, 1 for some and 2 or >2 for all punctuation."
  (catch 'return
    (cond
     ((booleanp punct)
      (if punct
	  (progn
	    (setq-local greader-speechd-punctuation "all")
	    (throw 'return (concat "-mall")))
	  (throw 'return greader-speechd-punctuation)))
     ((numberp punct)
      (if (= punct 0)
	  (progn
	    (setq-local greader-speechd-punctuation "none")
	    (throw 'return (concat "-mnone"))))
      (if (= punct 1)
	  (progn
	    (setq-local greader-speechd-punctuation "some")
	    (throw 'return (concat "-msome"))))
      (if (>= punct 2)
	  (progn
	    (setq-local greader-speechd-punctuation "all")
	    (throw 'return (concat "-mall"))))))))

(defun greader-speechd-stop ()
  "stops speech-dispatcher client."
  (start-process "speechd-client" nil greader-speechd-executable "-S")
  (sleep-for 0 100))
;;;###autoload
(defun greader-speechd (command &optional arg &rest _)
  "greader speech-dispatcher back-end."
  (pcase command
    ('executable
     greader-speechd-executable)
    ('lang
     (if (not arg)
	 (greader-speechd-set-language)
       (greader-speechd-set-language arg)))
    ('rate
     (cond((not arg)
	   (greader-speechd-set-rate))
	  ((numberp arg)
	   (greader-speechd-set-rate arg))
	  ((equal arg 'value)
	   greader-speechd-rate)))
    ('punctuation
     (cond
      ((equal arg 'no)
       (greader-speechd-set-punctuation 0)
       nil)
      ((equal arg 'yes)
       (greader-speechd-set-punctuation 2))
      ((not arg)
       (if (equal (greader-speechd-set-punctuation) "all")
"-mall"
	 nil))))
    ('stop
     (greader-speechd-stop))
    ('extra
     "-w")
    (_
     'not-implemented)))
(put 'greader-speechd 'greader-backend-name "greader-speechd")
(provide 'greader-speechd)
;;; greader-speechd.el ends here
