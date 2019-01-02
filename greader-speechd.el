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
