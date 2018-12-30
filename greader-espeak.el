;;; greader-espeak
;;; A greader's back-end for espeak synthesizer.
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
