;;; greader-espeak --- espeak back-end for greader -*- lexical-binding: t; -*-

					;Copyright (C) 2018 by Michelangelo Rodriguez
(defgroup greader-espeak-backend
nil
  "back-end of espeak for greader."
  :group 'greader
  )

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
