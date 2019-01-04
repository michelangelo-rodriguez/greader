;;; greader-speechd.el --- speech-dispatcher back-end for greader  -*- lexical-binding: t; -*-
					;Copyright (C) 2019 by Michelangelo Rodriguez


;;; customization variables
(defgroup greader-speechd
  nil
  "speech-dispatcher back-end for greader"
  :group 'greader)

(defcustom
  greader-speechd-executable
  "spd-say"
  "executable file name."
  :tag "speech-dispatcher client executable file name"
  :type 'string)
(defcustom
  greader-speechd-executable-path
  (greader-speechd--find-executable)
  "Path of speech-dispatcher client executable."
  :tag "speechd client executable path"
  :type 'string)
(defcustom
  greader-speechd-language
  "en"
  "specifies language of speech-dispatcher client to speak in."
  :tag "speech-dispatcher language"
  :type 'string)
(defcustom
  greader-speechd-rate
  10
  "specifies rate of speech.
(From -100 to 100.)"
  :tag "speech-dispatcher rate"
  :type 'integer)
(defcustom
  greader-speechd-punctuation
  "none"
"punctuation level of speech-dispatcher client to speak.
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
    (concat "-l " greader-speechd-language)
    (concat "-l " lang)))

(defun greader-speechd-set-rate
    (&optional rate)
  "returns parameter suitable for spd-say to set speech rate.
for further documentation, see the documentation for greader-speechd-rate variable."
  (if (not rate)
      (concat "-r " (number-to-string greader-speechd-rate))
    (concat "-r " (number-to-string rate))))
(defun greader-speechd-set-punctuation (&optional punct)
  "returns a suitable parameter to pass to spd-say for setting punctuation leve.
punct must be a numeric value, 0 for no punctuation, 1 for some and 2 or >2 for all punctuation."
(catch 'return
  (cond
   ((booleanp punct)
    (if punct
	(throw 'return (concat "-p all"))
      (throw 'return (concat "-p none"))))
   ((numberp punct)
     (if (= punct 0)
	 (throw 'return (concat "-p none")))
     (if (= punct 1)
	 (throw 'return (concat "-p some")))
     (if (>= punct 2)
	 (throw 'return (concat "-p all")))))))
(defun greader-speechd-stop ()
  "stops speech-dispatcher client."
  (start-process "speechd-client" nil greader-speechd-executable-path "-S"))
(defun greader-speechd (command &optional arg &rest ignore)
  "greader speech-dispatcher back-end."
  (pcase command
    ('executable
     greader-speechd-executable-path)
    ('lang
     (if (not arg)
	 (greader-speechd-set-language)
       (greader-speechd-set-language arg)))
    ('rate
     (if (not arg)
	 (greader-speechd-set-rate)
       (greader-speechd-set-rate arg)))
    ('punct
     (if (not arg)
	 (greader-speechd-set-punctuation)
       (greader-speechd-set-punctuation arg)))
    ('stop
     (greader-speechd-stop))
    (not-implemented
     'not-implemented)))
