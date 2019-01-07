;;; greader-espeak --- espeak back-end for greader -*- lexical-binding: t; -*-




;Copyright (C) 2019 by Michelangelo Rodriguez


(defgroup greader-espeak
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
  (locate-file "espeak" exec-path)
  "Path of espeak executable.
this variable determines authomatically if espeak is present in your PATH environment, then if this variable is nil, it means that you must first install espeak."
  :tag "espeak executable"
  :type 'string)
(defcustom
  greader-espeak-punctuation
  nil
  "espeak punctuation switch."
  :tag "espeak punctuation"
  :type 'boolean)
  
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
            (concat "-v" greader-espeak-language)
    (concat "-v " lang)))
(defun greader-espeak--find-executable
    ()
  "tries to find espeak executable in PATH.
If it's present, returns absolute path of espeak, else returns nil."
(locate-file "espeak" exec-path))
(defun greader-espeak-set-punctuation
    (&optional switch)
  "sets espeak punctuation on or off.
this function accepts only nil or t."
  (if switch
      "--punct"
    greader-espeak-punctuation))

(defun greader-espeak (command &optional arg &rest args)
  "back-end main function of greader-espeak."
  (pcase command
    ('executable
     (if greader-espeak-executable-name
	 greader-espeak-executable-name
       nil))
    ('lang
     (if (not arg)
	 (greader-espeak-set-language)
       (greader-espeak-set-language arg)))
    ('rate
     (if (not arg)
	 (greader-espeak-set-rate)
       (greader-espeak-set-rate arg)))
    ('punctuation
     (if (not arg)
	 (greader-espeak-set-punctuation)
       (greader-espeak-set-punctuation arg)))
    (command-not-implemented
     'not-implemented)))
(provide 'greader-espeak)
