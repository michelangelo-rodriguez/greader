;;; greader-espeak --- espeak back-end for greader -*- lexical-binding: t; -*-

					;Copyright (C) 2018 by Michelangelo Rodriguez
(defgroup greader-espeak-backend
nil
  "back-end of espeak for greader."
  :group 'greader
  )
;;; customization
(defcustom
  greader-espeak-language
  "en"
  "specifies the language of this back-end. For a comprehensive list of languages and voices available in espeak type in a terminal:
espeak --list-languages
"
  :tag "greader espeak language"
  :type 'string)
(defcustom
  greader-espeak-rate
  200
  "Specifies the rate os speech in words per minute."
  :tag "greader espeak rate"
  :type 'integer)
(defcustom
  greader-espeak-executable-name
(greader-espeak--find-executable)
"Path of espeak executable.
this variable determines authomatically if espeak is present in your PATH environment, then if this variable is nil, it means that you must first install espeak."
:tag "espeak executable"
:type 'string)
;;; code
(defun greader-espeak-set-rate
    (&optional rate)
  "returns a string suitable for setting espeak rate."
  (if (not rate)
      (concat "-s" (number-to-string greader-espeak-rate))
    (concat "-s" (number-to-string rate))))
(defun greader-espeak-set-language
    (&optional lang)
  "returns the appropriate string to pass to espeak in order to set the language appropriately"
  (if (not lang)
      
      (concat "-V " greader-espeak-language)
    (concat "-V " lang)))
(defun greader-espeak--find-executable
    ()
  "tries to find espeak executable in PATH.
If it's present, returns absolute path of espeak, else returns nil."
(locate-file "espeak" exec-path))
