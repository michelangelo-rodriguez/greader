;;; greader.el --- gnamù reader, a reader with espeak tts  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019  Free Software Foundation, Inc.

;; package-requires: ((emacs "25"))
;; Author: Michelangelo Rodriguez <michelangelo.rodriguez@gmail.com>
;; Keywords: tools

;; Version: 0.1

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

;;; Commentary:

;; To start using greader, you have to install espeak and/or speech-dispatcher,
;; and make sure those packages work correctly.

;; In order to read a buffer:
;; 'M-x greader'
;; 'C-r <spc>'

;;; Code:





(defvar greader-spaces '(" " "\t"))
(defvar greader-hyphenation-newlines '("\r" "\n"))
(defvar greader-hyphenation-symbol '("-" "‐"))
(defvar greader-auto-tired-timer nil)
(defvar greader-auto-tired-end-timer)
(defvar greader-last-point nil)
(defvar greader-tired-timer nil)
(defvar greader-timer-enabled-interactively nil)
(defvar greader-stop-timer 0)
(defvar greader-elapsed-timer 0)
(defvar greader-elapsed-time 0)
(defvar greader-timer-flag nil)
(defvar greader-tired-flag nil)
(defvar greader-filter-enabled t)
(defvar point-limit nil)
(defvar greader-differs nil)
(defvar greader-not-start-of-sentence '(" " "\n" "\t"))
(defvar greader-end-of-sentence '("." "?" "!" ":"))
(defvar greader-debug-buffer "spd-output"
  "contains the buffer name for debugging purposes.")
(defvar greader-backend-action 'greader--default-action)
(defvar greader-status 'paused)
(defvar greader-synth-process nil)
(require 'seq)

(defgroup
  greader
  nil
  "greader customization"
  :group 'convenience)

(defcustom
  greader-backends
  '(greader-espeak greader-speechd)
  "A list of functions that are back-ends for greader."
  :tag "greader back-ends"
  :type '(repeat function))
(defcustom
  greader-actual-backend
  'greader-espeak
  "greader back-end to use"
  :tag "greader actual back-end"
  :type
  `(radio
    ,@(mapcar
       (lambda (backend)
	 `(function-item ,backend))
       greader-backends)))
(defcustom
  greader-auto-tired-mode-time
  "22"
  "specifies the hour when tired mode will be activated automatically."
  :tag "greader-tired-mode start time"
  :type 'string)

(defcustom
  greader-auto-tired-time-end
  "07"
  "specifies when auto tired mode should be disabled.
For more information on syntax, see documentation of greader-auto-tired-mode-time."
  :tag "greader auto tired end time"
  :type 'string)

(defcustom
  greader-auto-tired-mode
  nil
  "if t, auto-tired-mode is enabled, and tired mode will be turned up at a time specified wit greader-auto-tired-mode-time and disabled at greader-auto-tired-mode-end-time automatically."
  :tag "greader auto tired mode"
  :type 'boolean)

(defcustom
  greader-tired-time
  60
  "sets when, in seconds after a timer stop, the point must be moved to the last position have you called greader-read."
  :type 'integer
  :tag "greader seconds for tired mode")
(defcustom
  greader-soft-timer
  t
  "if enabled, reading of text will end not exactly at time expiration but after the end of sentence actually spoken."
  :tag "greader soft timer"
  :type 'boolean)
(defcustom
  greader-timer
  10
  "minutes for timer to stop reading"
  :type 'integer
  :tag "time to stop reading")

(defcustom
  greader-debug
  nil
  "enables debug information"
  :tag "enable debug"
  :type 'boolean)

(defcustom
  greader-hook
  nil
  "hook ran after mode activation.
through this hook you can customize your key definitions for greader, for example."
  :tag "greader-mode hook"
  :type 'hook)

(defcustom greader-move-to-next-chung
  'greader-forward-sentence
  "sets the function that moves the cursor for the next chung of text.
For example if you have specified `sentence-at-point' function to get the actual chung, you should specify `forward-sentence' for this variable."
  :tag "greader move to next chung function"
  :type 'function)

(defcustom greader-read-chung-of-text
  'greader-sentence-at-point
  "sets the function used to get the portion of text to read.
the variable `greader-move-to-next-chung' must be set to a function that moves the cursor to the same amount of text that is set in this variable.
For example, if you specify a function that gets a sentence, you should specify a function that moves to the next one."
  :type 'function
  :tag "greader get chung of text function")
(defvar greader-prefix-map (make-sparse-keymap))
(defvar greader-map (make-sparse-keymap))
(defvar greader-reading-map (make-sparse-keymap))

(define-key greader-map (kbd "C-r s") 'greader-toggle-tired-mode)
(define-key greader-prefix-map (kbd "C-r") greader-map)
(define-key greader-map (kbd "C-r r") 'isearch-backward)
(define-key greader-map (kbd "C-r SPC") 'greader-read)
(define-key greader-reading-map (kbd "SPC") 'greader-stop)
(define-key greader-map (kbd "C-r l") 'greader-set-language)
(define-key greader-reading-map (kbd "p") 'greader-toggle-punctuation)
(define-key greader-reading-map (kbd ".") 'greader-stop-with-timer)
(define-key greader-map (kbd "C-r t") 'greader-toggle-timer)
(define-key greader-reading-map (kbd "+") 'greader-inc-rate)
(define-key greader-reading-map (kbd "-") 'greader-dec-rate)
(define-key greader-map (kbd "C-r f") 'greader-get-attributes)
(define-key greader-map (kbd "C-r b") 'greader-change-backend)
(define-minor-mode greader-mode
  nil
  nil
  " greader"
  greader-map
  :group greader
  (if greader-mode
      (if greader-auto-tired-mode
	  (greader-auto-tired-mode-setup))
    (if greader-auto-tired-timer
	(progn
	  (cancel-timer greader-auto-tired-timer)
	  (greader-toggle-timer))))
  (greader-load-backends))
;;;code
(defun greader--get-backends ()
  "returns actual available back-ends, as a list of strings."
  (let (b)
    (dolist (greader-elem greader-backends nil)
      (setq b (append b `(,(get greader-elem 'greader-backend-name)))))
    b))

(defun greader-call-backend (command &optional arg)
  (if arg
      (funcall greader-actual-backend command arg)
    (funcall greader-actual-backend command)))
(defvar
  greader-backend-filename
  (greader-call-backend 'executable))
(defvar greader-backend `(,greader-backend-filename))

(defun greader-change-backend (&optional backend)
  "changes back-end. if backend is specified, it changes to backend, else it cycles throwgh available back-ends."
  (interactive
   (list
    (if current-prefix-arg
	(completing-read"back-end:" (greader--get-backends)))))
  (if (functionp backend)
      (if (memq backend greader-backends)
	  (setq-local greader-actual-backend backend)
	(error "%s" "The function you have specified is not a greader's back-end.")))
  (if (stringp backend)
      (progn
	(let ((result nil))
	  (dolist (elem greader-backends result)
	    (if
		(equal
		 (get elem 'greader-backend-name) backend)
		(setq result elem)))
	  (if result
	      (setq-local greader-actual-backend result)
	    (error "%s" "the function name you have specified is not a greader's back-end.")))))
  (if (not backend)
      (let
	  ((index (seq-position greader-backends greader-actual-backend))
	   (len (length greader-backends)))
	(if
	    (= (+ index 1) len)
	    (setq-local greader-actual-backend (elt greader-backends 0))
	  (setq-local greader-actual-backend (elt greader-backends (+ index 1))))))
  (message "Actual back-end is %s." (get greader-actual-backend 'greader-backend-name)))

(defun greader-load-backends ()
  "loads backends taken from greader-backends."
  (mapcar 'require greader-backends))

(defun greader-read-asynchronous (txt)
  "reads the text given in txt."
  (if greader-debug
      (greader-debug "greader-read-asynchronous entered\n"))
  (greader-build-args)
  (if (and txt (greader-sentence-needs-dehyphenation txt))
      (setq txt (greader-dehyphenate txt)))
  (let (backend text)
    (setq text (concat text " "))
    (setq text (concat text txt))
    (setq txt text)
    (setq backend (append greader-backend `(,txt) backend))
    (and (stringp txt) (setq-local greader-synth-process (make-process
							  :name "greader-backend"
							  :sentinel 'greader-action
							  :filter 'greader-process-filter
							  :command backend)))
    (if greader-debug
	(progn
	  (set-process-buffer greader-synth-process greader-debug-buffer)
	  (greader-debug (message "greader-read-asynchronous: %S" backend))))))

(defun greader-get-status ()
  greader-status)

(defun greader-change-status (arg)
  (if greader-debug
      (greader-debug "greader-change-status entered"))
  (if (symbolp arg)
      (setq greader-status arg)
    (error "status must be a symbol!")))

(defun greader-action (process event)
  (if greader-debug
      (progn
	(greader-debug "greader-action entered.\n")
	(greader-debug (format "event: %S\n" event))))
  (when greader-backend-action
    (funcall greader-backend-action process event)))

(defun greader-tts-stop ()
  (set-process-sentinel greader-synth-process 'greader--default-action)
  (if
      (not
       (eq
	(greader-call-backend 'stop) 'not-implemented))
      (greader-call-backend 'stop))
  (delete-process greader-synth-process)
  (setq-local greader-backend-action 'greader--default-action))

(defun greader--default-action (&optional process event)
  (if greader-debug
      (greader-debug (format "greader--default-action entered.\nevent: %S\n" event)))
  (cond
   ((and (greader-timer-flag-p) (timerp greader-stop-timer))
    (greader-cancel-elapsed-timer)
    (greader-cancel-stop-timer)
    (greader-reset-elapsed-time)
    (setq-local greader-stop-timer 0)
    (greader-set-greader-keymap))))

(defun greader-build-args ()
  (greader-reset)
  (let (args arg)
    (setq arg
	  (greader-call-backend 'rate))
    (setq args (append `(,arg) args))
    (cond ((greader-call-backend 'lang)
	   (setq arg
		 (greader-call-backend 'lang))
	   (setq args (append `(,arg) args))))
    (cond ((greader-call-backend 'punctuation)
	   (setq arg (greader-call-backend 'punctuation))
	   (setq args (append `(,arg) args))))
    (setq greader-backend (greader-call-backend 'executable))
    (cond
     (
      (not
       (eq
	(greader-call-backend 'extra)
	'not-implemented))
      (setq arg (greader-call-backend 'extra))
      (setq args (append `(,arg) args))))
    (setq greader-backend (append `(,greader-backend) args))))

(defun greader-reset ()
  (setq greader-backend `(,(greader-call-backend 'executable))))
(defun greader-next-action (process event)
  (if greader-debug
      (greader-debug (format "greader-next-action: %s" event)))
  (funcall greader-move-to-next-chung)
  (funcall 'greader-read))

(defun greader-read ()
  "starts reading of current buffer."
  (interactive)
  (if (and greader-tired-flag (= greader-elapsed-time 0))
      (progn
	(if greader-tired-timer
	    (cancel-timer greader-tired-timer))
	(setq-local greader-last-point (point))))
  (cond
   ((and (greader-timer-flag-p) (not (timerp greader-stop-timer)))
    (greader-setup-timers)))
  (let ((chung (funcall greader-read-chung-of-text)))
    (if chung
	(progn
	  (greader-set-reading-keymap)
	  (setq-local greader-read 'greader-read)
	  (setq-local greader-backend-action 'greader-next-action)
	  (greader-read-asynchronous chung))
      (progn
	(setq-local greader-backend-action 'greader--default-action)
	(greader-set-greader-keymap)
	(greader-read-asynchronous ". end")))))

(defun greader-set-reading-keymap ()
  (if (assoc 'greader-mode minor-mode-map-alist)
      (progn
	(setq minor-mode-map-alist (assq-delete-all 'greader-mode minor-mode-map-alist))
	(setq minor-mode-map-alist (push `(greader-mode . ,greader-reading-map) minor-mode-map-alist)))))

(defun greader-set-greader-keymap ()
  (if (assoc 'greader-mode minor-mode-map-alist)
      (progn
	(setq minor-mode-map-alist (assq-delete-all 'greader-mode minor-mode-map-alist))
	(setq minor-mode-map-alist (push `(greader-mode . ,greader-map) minor-mode-map-alist)))))

(defun greader-stop ()
  "stops reading of document."
  (interactive)
  (cond
   ((and (> greader-elapsed-time 0) greader-timer-flag)
    (greader-cancel-elapsed-timer)
    (greader-cancel-stop-timer)
    (if (>= greader-elapsed-time (1- (greader-convert-mins-to-secs greader-timer)))
	(greader-reset-elapsed-time))
    (setq-local greader-stop-timer 0)))
  (greader-set-greader-keymap)
  (greader-tts-stop))
(defun greader-debug (arg)
  (save-current-buffer
    (get-buffer-create greader-debug-buffer)
    (set-buffer greader-debug-buffer)
    (insert arg)))

(defun greader-punct-p (arg)
  (if (member arg greader-end-of-sentence)
      t
    nil))

(defun greader-next-sentence (&optional direction)
  (if (not direction)
      (setq direction 1))
  (if (< direction 0)
      (progn
	(setq point-limit 'point-min)
	(setq direction '-)
	(setq greader-differs '>))
    (progn
      (setq point-limit 'point-max)
      (setq direction '+)
      (setq greader-differs '<)))
  (catch 'afterloop
    (save-excursion
      (while (funcall greader-differs (point) (funcall point-limit))
	(cond
	 ((greader-end-sentence-p)
	  (goto-char (funcall direction (point) 1))
	  (while (member (string (following-char)) greader-not-start-of-sentence)
	    (goto-char (funcall direction 1 (point))))
	  (throw 'afterloop (point))))
	(goto-char (funcall direction (point) 1))))
    (funcall point-limit)))

(defun greader-forward-sentence ()
  (goto-char (greader-next-sentence)))

(defun greader-get-sentence (&optional direction)
  (if (not direction)
      (setq direction 1))
  (if (< direction 0)
      (progn
	(setq direction '-)
	(setq point-limit 'point-min)
	(setq greader-differs '>))
    (progn
      (setq direction '+)
      (setq point-limit 'point-max)
      (setq greader-differs '<)))

  (let (sentence)
    (catch 'afterloop
      (save-excursion
	(while (funcall greader-differs (point) (funcall point-limit))
	  (setq sentence (concat sentence (string (following-char))))
	  (cond
	   ((greader-end-sentence-p)
	    (throw 'afterloop sentence)))
	  (goto-char (funcall direction (point) 1)))
	sentence))))

(defun greader-sentence-at-point ()
  (greader-get-sentence))

(defun greader-end-sentence-p ()
  (catch 'endsentence
    (save-excursion
      (if (eobp)
	  (throw 'endsentence t))
      (if
	  (and
	   (greader-punct-p (string (following-char)))
	   (progn
	     (goto-char (+ (point) 1))
	     (member (string (following-char)) greader-not-start-of-sentence)))
	  t
	nil))))

(defun greader-process-filter (process string)
  (if greader-filter-enabled
      (message string)))

(defun greader-set-language (lang)
  "sets language of tts.
The language must be in ISO code, for example 'en' for english or 'fr' for french.
This function sets the language of tts local for current buffer, so if you want to set it globally, please use 'm-x customize-option <RET> greader-language <RET>'."
  (interactive "sset language to:")
  (greader-call-backend 'lang lang))
(defun greader-set-punctuation (flag)
  (greader-call-backend 'punctuation flag))

(defun greader-toggle-punctuation ()
  "Toggles punctuation locally for current buffer."
  (interactive)
  (if (not (greader-call-backend 'punctuation))
      (progn
	(greader-stop)
	(greader-set-punctuation 'yes)
	(message "punctuation enabled in current buffer")
	(greader-read))
    (progn
      (greader-stop)
      (greader-set-punctuation 'no)
      (message "punctuation disabled in current buffer")
      (greader-read))))

(defun greader-toggle-timer-flag ()
  (cond
   (greader-timer-flag
    (setq-local greader-timer-flag nil)
    (greader-reset-elapsed-time)
    (if (not (equal greader-elapsed-timer 0))
	(greader-cancel-elapsed-timer))
    (if (and greader-auto-tired-mode greader-tired-flag)
	(greader-toggle-tired-mode)))
   ((not greader-timer-flag)
    (setq-local greader-timer-flag t))))

(defun greader-toggle-timer ()
  (interactive)
  (greader-toggle-timer-flag)
  (if greader-timer-flag
      (progn
	(setq-local greader-timer-enabled-interactively t)
	(message "timer enabled in current buffer"))
    (progn
      (setq-local greader-timer-enabled-interactively nil)
      (if greader-tired-flag
	  (greader-toggle-tired-flag))
      (message "timer disabled in current buffer"))))

(defun greader-set-timer (&optional timer-in-mins)
  "sets timer for reading expressed in minutes.
This command should be used only if you want to set locally a timer different of that you set via customize, that is considered the default value for this variable."

  (interactive "Nset timer for:")
  (if (not (greader-timer-flag-p))
      (greader-toggle-timer))
  (setq-local greader-timer timer-in-mins))

(defun greader-timer-flag-p ()
  (if greader-timer-flag
      t
    nil))

(defun greader-setup-timers ()
  "set up timers, that is, call run-at-time using settings you have specified."
  (catch 'timer-is-nil
    (cond
     ((greader-timer-flag-p)
      (setq-local greader-stop-timer (run-at-time (- (greader-convert-mins-to-secs greader-timer) greader-elapsed-time) nil 'greader-stop-timer-callback))
      (setq-local greader-elapsed-timer (run-at-time 1 1 'greader-elapsed-time)))
     ((not (greader-timer-flag-p))
      (throw 'timer-is-nil nil))))
  t)

(defun greader-elapsed-time ()
  (setq-local greader-elapsed-time (1+ greader-elapsed-time)))

(defun greader-convert-mins-to-secs (mins)
  (* mins 60))

(defun greader-cancel-stop-timer ()
  (cancel-timer greader-stop-timer))

(defun greader-cancel-elapsed-timer ()
  (cancel-timer greader-elapsed-timer))

(defun greader-reset-elapsed-time ()
  (setq-local greader-elapsed-time 0))

(defun greader-stop-with-timer ()
  "stops reading of buffer and also reset timer.
If you use this command, next reading will start timer at its current value.
If you stops normally with greader-stop, next reading will continue from the time elapsed before you stopped."
  (interactive)
  (if (greader-timer-flag-p)
      (progn
	(greader-cancel-elapsed-timer)
	(greader-cancel-stop-timer)
	(setq-local greader-stop-timer 0)
	(greader-reset-elapsed-time)))
  (greader-stop))

(defun greader-stop-timer-callback ()
  "function called when timer expires."
  (if greader-tired-flag
      (greader-setup-tired-timer))
  (cond
   ((greader-soft-timer-p)
    (setq-local greader-backend-action 'greader--default-action))
   ((not greader-soft-timer)
    (greader-stop))))

(defun greader-soft-timer-p ()
  (if greader-soft-timer
      t
    nil))

(defun greader-toggle-tired-flag ()
  (if greader-tired-flag
      (progn
	(if greader-timer-flag
	    (greader-toggle-timer))
	(setq-local greader-tired-flag nil))
    (progn
      (if (not greader-timer-flag)
	  (greader-toggle-timer)))
    (setq-local greader-tired-flag t)))

(defun greader-toggle-tired-mode ()
  "toggles tired mode.
if tired mode is enabled, when a timer expires, greader will wait an amount of time customizable, and if emacs remains iddle for that time, point in buffer will be placed at last position where you called greader-read.
Enabling tired mode implicitly enables timer also."
  (interactive)
  (if (not greader-tired-flag)
      (progn
	(greader-toggle-tired-flag)
	(if (not (greader-timer-flag-p))
	    (greader-toggle-timer-flag))
	(message "tired mode enabled in current buffer."))
    (progn
      (if (not greader-timer-enabled-interactively)
	  (greader-toggle-timer-flag))

      (greader-toggle-tired-flag)
      (message "tired mode disabled in current buffer"))))

(defun greader-setup-tired-timer ()
  (if greader-tired-flag
      (run-with-idle-timer
       (time-add
	(current-idle-time)
	(seconds-to-time
	 greader-tired-time)) nil 'greader-tired-mode-callback)))

(defun greader-tired-mode-callback ()
  (if (equal last-command 'greader-read)
      (greader-move-to-last-point)))

(defun greader-move-to-last-point ()
  (goto-char greader-last-point))

(defun greader-auto-tired-mode-setup ()
  (if greader-auto-tired-mode
      (progn
	(if (not greader-tired-flag)
	    (greader-toggle-tired-mode))
	(setq-local greader-auto-tired-timer(run-at-time nil 1 'greader-auto-tired-callback)))
    (progn
      (if greader-tired-flag
	  (greader-toggle-tired-mode))
      (setq-local greader-auto-tired-timer (cancel-timer greader-auto-tired-timer)))))
(defun greader-toggle-auto-tired-mode-flag ()
  (if greader-auto-tired-mode
      (progn
	(setq-local greader-auto-tired-mode nil)
	(if greader-auto-tired-timer
	    (cancel-timer greader-auto-tired-timer)))
    (progn
      (setq-local greader-auto-tired-mode t)
      (greader-auto-tired-mode-setup))))

(defun greader-toggle-auto-tired-mode ()
  "Enables auto tired mode.
In this mode, greader will enter in tired mode at a customizable time and will exit from it at another time.
The default is 22:00 for entering and 08:00 for exiting."
  (interactive)
  (greader-toggle-auto-tired-mode-flag)
  (if greader-auto-tired-mode
      (message "auto-tired mode enabled in current buffer")
    (message "auto-tired mode disabled in current buffer.")))

(defun greader-current-time ()
  (string-to-number (format-time-string "%H")))

(defun greader-convert-time (time)
  (let ((current-t (decode-time))
	(i (nth 2 (decode-time)))
	(counter (nth 2 (decode-time))))
    (if (stringp time)
	(setq time (string-to-number time)))
    (catch 'done
      (while t
	(if (= i time)
	    (throw 'done nil))
	(cl-incf i)
	(cl-incf counter)
	(if (= i 24)
	    (setq i 0))))
    (setcar (cdr (cdr current-t)) counter)
    (setcar current-t 0)
    (setcar (cdr current-t) 0)
    (apply 'encode-time current-t)))

(defun greader-current-time-in-interval-p (time1 time2)
  (let
      ((current-t (current-time)))
    (if
	(and (time-less-p time1 current-t) (time-less-p current-t time2))
	t
      nil)))

(defun greader-auto-tired-callback ()
  (if
      (stringp greader-auto-tired-mode-time)
      (setq-local greader-auto-tired-mode-time (greader-convert-time greader-auto-tired-mode-time)))
  (if
      (stringp greader-auto-tired-time-end)
      (setq-local greader-auto-tired-time-end (greader-convert-time greader-auto-tired-time-end)))
  (if
      (and
       (greader-current-time-in-interval-p greader-auto-tired-mode-time greader-auto-tired-time-end)
       (not greader-tired-flag))
      (greader-toggle-tired-mode))
  (if
      (and
       (not (greader-current-time-in-interval-p greader-auto-tired-mode-time greader-auto-tired-time-end))
       greader-tired-flag)
      (progn
	(setq-local greader-auto-tired-mode-time (number-to-string (nth 2 (decode-time greader-auto-tired-mode-time))))
        (setq-local greader-auto-tired-time-end (number-to-string (nth 2 (decode-time greader-auto-tired-time-end))))
	(greader-toggle-tired-mode))))

(defun greader-set-rate (n)
  "sets rate in current buffer to tthe specified value in n. rate is expressed in words per minute.
For maximum value, see 'man espeak'."
  (greader-call-backend 'rate n))


(defun greader-inc-rate (&optional n)
  "increments rate of speech by 10 units.
If prefix, it will be used to increment by that."
  (interactive "P")
  (if (not n)
      (setq n 10))
  (greader-stop)
  (greader-set-rate (+ (greader-call-backend 'rate 'value) n))
  (greader-read))

(defun greader-dec-rate (&optional n)
  "decrements rate of speech by units specified in n.
If prefix, it will be used to decrement  rate."
  (interactive "P")
  (if (not n)
      (setq n 10))
  (greader-stop)
  (greader-set-rate (- (greader-call-backend 'rate 'value) n))
  (greader-read))

(defun greader-sentence-needs-dehyphenation (str)
  "returns t if str has lines iphenated."
  (let
      ((i 0)
       (j 0))
    (catch 'done
      (while (< i (length str))
	(if (and (member (string (aref str i)) greader-hyphenation-symbol)
		 (member (string (aref str (1+ i))) greader-hyphenation-newlines))
	    (progn
	      (setq j 1)
	      (throw 'done t)))
	(cl-incf i))
      (if (= j 0)
	  nil))))

(defun greader-dehyphenate (str)
  "dehyphenates str.
new lines can be either in unix stile, or ms, or macosX."
  (let
      ((new-sentence "")
       (i 0)
       )
    (while (< i (length str))
      (catch 'done
	(if
	    (and
	     (member (string (aref str i)) greader-hyphenation-symbol)
	     (member (string (aref str (+ i 1))) greader-hyphenation-newlines))
	    (progn
	      (cl-incf i)
	      (while (or (member (string (aref str i)) greader-hyphenation-newlines)
			 (member (string (aref str i)) greader-spaces))
		(cl-incf i))
	      (throw 'done nil)))
	(setq new-sentence (concat new-sentence (string (aref str i))))
	(cl-incf i)))
    new-sentence))
(defun greader-get-attributes ()
  "prints text properties associated with current char."
  (interactive)
  (print (text-properties-at (point))))
(provide 'greader)
;;; greader.el ends here
