;;; greader-espeak --- espeak back-end for greader -*- lexical-binding: t; -*-




					;Copyright (C) 2019 by Michelangelo Rodriguez


(defgroup greader-espeak
  nil
  "back-end of espeak for greader."
  :group 'greader
  )
;;; customization
(defcustom greader-espeak-language "en"
  "specifies the language of this back-end. For a comprehensive list of languages and voices available in espeak type in a terminal:
espeak --list-languages"
  :tag "greader espeak language"
  :type 'string)

(defcustom greader-espeak-rate 200
  "Specifies the rate os speech in words per minute."
  :tag "greader espeak rate"
  :type 'integer)

(defcustom greader-espeak-executable-name "espeak"
  "File name of espeak executable.
this variable determines authomatically if espeak is present in your PATH environment, then if this variable is nil, it means that you must first install espeak."
  :tag "espeak executable"
  :type 'string)

(defcustom greader-espeak-punctuation nil
  "espeak punctuation switch."
  :tag "espeak punctuation"
  :type 'boolean)

;;; code
(defun greader-espeak-set-rate (&optional rate)
  "Return a string suitable for setting espeak rate."
  (if (not rate)
      (concat "-s" (number-to-string greader-espeak-rate))
    (progn
      (setq-local greader-espeak-rate rate)
      (concat "-s" (number-to-string rate)))))

(defun greader-espeak-set-language (&optional lang)
  "Return the appropriate string to pass to espeak in order to set the language appropriately"
  (if (not lang)
      (concat "-v" greader-espeak-language)
    (progn
      (setq-local greader-espeak-language lang)
      (concat "-v " lang))))

;;;###autoload
(defun greader-espeak (command &optional arg &rest _)
  "back-end main function of greader-espeak."
  (pcase command
    ('executable
     greader-espeak-executable-name)
    ('lang
     (greader-espeak-set-language arg))
    ('rate
     (cond
      ((equal arg 'value)
       greader-espeak-rate)
      (t
       (greader-espeak-set-rate arg))))
    ('punctuation
     (pcase arg
       ('yes
        (setq-local greader-espeak-punctuation t)
        "--punct")
       ('no
        (setq-local greader-espeak-punctuation nil)
        nil)
       ('nil
        (if greader-espeak-punctuation
	    "--punct"
	  nil))))
    
    (_
     'not-implemented)))

(provide 'greader-espeak)
