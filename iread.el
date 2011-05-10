;; iread.el --- incremental X minor mode

;; Copyright (C) 2010 Paul M. Rodriguez

;; Author: Paul M. Rodriguez <paulmrodriguez@gmail.com>
;; Created: 2010-10-11
;; Version: 1.1
;; Keywords: emacs

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Iread is Isearch without the search---a utility for acting
;; incrementally on user input.  It is implemented as a minor mode,
;; with a number of normal and abnormal hooks.  But all the moving
;; parts are encased by the macro `defiread', which lets you build up
;; incremental behavior by passing keyword arguments.

;;; Code:

(eval-when-compile (require 'cl))

(defvar iread-lighter ""
  "Display in mode line while reading.")
(defvar iread-prompt "Type: "
  "Display as prompt in minibuffer.")
(defvar iread-input nil
  "Accumulate input.")
(defvar iread-minibuffer-message nil
  "Display after minibuffer contents.")
(defvar iread-ignore-prefix nil
  "Ignore input that begins with this.")
(defvar iread-delay 0
  "Wait this many seconds.")
(defvar iread-min 0
  "Require this many characters.")

(defvar iread-mode-hook nil
  "Run before and after `iread-mode'.")
(defvar iread-on-hook nil
  "Run before `iread-mode'.")
(defvar iread-off-hook nil
  "Run after `iread-mode'.")
(defvar iread-after-input-functions nil
  "Do something with `iread-input'.")
(defvar iread-exit-functions nil
  "Call with `iread-input' to exit Iread.")
(defvar iread-ignore-functions nil
  "Call with `iread-input' to ignore further input.")
(defvar iread-require-functions nil
  "Call with `iread-input' to determine if quitting is permitted.")

(defun iread-build-minibuffer ()
  "Build minibuffer contents.
Concatenate `iread-prompt', `iread-input', and
`iread-minibuffer-message'."
  (concat (apply 'propertize
		 iread-prompt
		 minibuffer-prompt-properties)
	  iread-input " "
	  iread-minibuffer-message))

(defun iread-backspace ()
  "Delete the last character of input.
Remove the last character from `iread-input'."
  (interactive)
  (setq iread-input
	(or
	 (condition-case nil
	     (substring iread-input 0 -1)
	   (args-out-of-range nil))
	 iread-input))
  (iread-update)
  (iread-operate))

(defun iread-may-exit-p ()
  "Succeed if exit is permitted.
Return non-nil if `iread-input' passes
`iread-require-functions'."
  (if iread-require-functions
   (run-hook-with-args-until-success
    'iread-require-functions iread-input) t))

(defun iread-ignore-p ()
  "Succeed if input should be ignored.
Return nil if any function in `iread-ignore-functions' succeeds
with `iread-input'."
  (or
   (and iread-ignore-prefix
	(string-prefix-p iread-ignore-prefix iread-input))
   (and (run-hook-with-args-until-success
	 'iread-ignore-functions iread-input)
	(setq iread-ignore-prefix iread-input))
   (setq iread-ignore-prefix nil)))

(defun iread-min-length-p ()
  "Fail if input is too short.
Return non-nil if `iread-input' meets or exceeds `iread-min'."
  (if (>= (length iread-input) iread-min) t
    (setq iread-minibuffer-message nil)))

(defun iread-delay-elapsed-p ()
  "Fail if user types too fast.
Return non-nil `iread-delay' has elapsed."
  (let ((message-log-max nil))
    ;; prevents flickering
    (with-temp-message (iread-build-minibuffer)
      (sit-for iread-delay t))))

(defun iread-operate ()
  "Operate on incremental input.
Maybe run `iread-after-input-functions' on `iread-input'."
  (and (iread-min-length-p)
       (not (iread-ignore-p))
       (run-hook-with-args 'iread-after-input-functions iread-input))
  (iread-update))

(defun iread-append ()
  "Append to input.
Append `last-command-event' to `iread-input'."
  (interactive)
  (setq iread-input
	(concat iread-input (char-to-string last-command-event)))
  (iread-update)
  (iread-operate))

(defun iread-unread-and-exit ()
  "Unread last event and exit Iread.
Push `last-command-event' to `unread-command-events' and exit
recursive edit.

This function discards any down events it receives so the
corresponding up events can be read."
  (interactive)
  (let ((ev last-command-event))
    (if (memq 'down (event-modifiers ev))
	(setq this-command 'undefined)
      (progn
	(setq unread-command-events ev)
	(iread-return)))))

(defun iread-edit ()
  "Edit input non-incrementally.
Pass `iread-input' to `read-string' as an initial input and set
`iread-input' to the result."
  (interactive)
  (let ((overriding-terminal-local-map minibuffer-local-map))
    (setq iread-input
	  ;; A justifiable case of initial input.
	  (read-string (concat iread-prompt "[EDIT] ") iread-input)))
  (iread-update)
  (iread-operate))

(defun iread-minibuffer-message (format-string &rest args)
  "Pass FORMAT-STRING and ARGS to `format' and show after minibuffer contents.
Wrap with [...] if needed."
  (interactive)
  (let (msg)
    (if (or (null format-string)
	    (string-equal format-string ""))
	(setq msg nil)
      (setq msg (apply 'format format-string args))
      (unless (string-prefix-p "[" msg)
	(setq msg (concat "[" msg "]"))))
    (setq iread-minibuffer-message msg)
    (iread-update)
    msg))

(defun iread-update ()
  "Update minibuffer display."
  (if (run-hook-with-args-until-success
       'iread-exit-functions
       iread-input)
      (iread-return)
    (let ((message-log-max nil))
      (message "%s" (iread-build-minibuffer)))))

(defun iread-define-keymap (map)
  "Return MAP as a keymap.
If MAP is already a keymap, just return it.

Otherwise, if MAP is a list, pass it to
`easy-mmode-define-keymap'."
  (cond ((keymapp map) map)
	((listp map) (easy-mmode-define-keymap map))
	(t (error "Invalid keymap %s" map))))

(defun iread-adopt-map (map)
  "Return a copy of MAP with `iread-mode-map' as its parent."
  (let ((map (copy-keymap map)))
    (set-keymap-parent map iread-mode-map)
    map))

(defun iread-prep-map (map)
  "Return a map suitable for `iread-mode'.
If MAP is non-nil, return a copy of it with `iread-mode-map' as
its parent.

If MAP is nil, return `iread-mode-map'."
  (if map
   (iread-adopt-map
    (iread-define-keymap map))
   iread-mode-map))

(defun iread-hook-value-p (form)
  "Return non-nil is FORM is a function or list of functions."
  (or (functionp form)
      (loop for fun in form always (functionp fun))))

(defun iread-make-hook-value (form &optional args)
  "Return FORM as a suitable value for a hook.
If FORM is already a function or a list of functions, just return
it.

Otherwise, wrap FORM in a function.  If ARGS is provided, the
function will use ARGS as its argument list; otherwise it takes
no parameters.  If ARGS is a symbol, that will be the one
parameter."
  (cond
   ((iread-hook-value-p form) form)
   ((and args (listp form))
    (setq args (if (listp args) args (list args)))
    `(lambda ,args ,form))
   ((and (null args) (listp form))
    `(lambda nil ,form))
   (t (error "No function"))))

(defun* iread-return (&optional (val iread-input))
  "Exit Iread.  Return VAL if provided, `iread-input' otherwise."
  (setq iread-input val)
  (iread-mode 0))

(defun iread-exit ()
  "End Iread.  Return `iread-input'."
  (interactive)
  (if (iread-may-exit-p)
      (iread-return)))

(defun iread-quit ()
  "Abort Iread.  Return nil."
  (interactive)
  (setq iread-input nil)
  (abort-recursive-edit))

(defvar iread-mode-map
  (let ((map (make-keymap)))
    ;; Bind all printable chars to 'iread-append.
    (map-char-table
     (lambda (k v)
       (set-char-table-range (nth 1 map) k 'iread-append))
     printable-chars)
    ;; Everything not explicitly bound should quit.
    (define-key map [t] 'iread-unread-and-exit)
    (define-key map "\C-e" 'iread-edit)
    (define-key map "\C-g" 'iread-quit)
    (define-key map "\C-h" 'help-command)
    (define-key map "\C-u" 'universal-argument)
    (define-key map [return] 'iread-exit)
    (define-key map [backspace] 'iread-backspace)
    map)
  "Current keymap for `iread-mode'.")

;;;###autoload
(define-minor-mode iread-mode
  "Incremental X minor mode.
Do not use this minor mode directly; use `iread' instead."
  :init-value nil
  :global t
  :lighter iread-lighter
  :keymap iread-mode-map
  (if iread-mode
      (progn
	;; This must be run before the recursive edit.
	(run-hooks 'iread-on-hook)
	(iread-update)
	(recursive-edit))
    (ignore-errors (exit-recursive-edit))
    (run-hooks 'iread-off-hook)))

;;;###autoload
(defmacro* defiread
    (name
     args
     doc
     prompt
     var
     &key
     (lighter "")
     (initial "")
     default
     do
     keymap
     first
     until
     unless
     last
     result
     require
     (delay idle-update-delay)
     (min 0))
  "Define an incremental read."
  (declare (indent defun))
  `(fset ',name
	 (lambda ,args ,doc
	   (let ((iread-prompt ,prompt)
		 (iread-lighter ,lighter)
		 (iread-input ,initial)
		 (iread-on-hook
		  (iread-make-hook-value ,first))
		 (iread-off-hook
		  (iread-make-hook-value ,last))
		 (iread-after-input-functions
		  (iread-make-hook-value ,do ,var))
		 (iread-exit-functions
		  (iread-make-hook-value ,until ,var))
		 (iread-ignore-functions
		  (iread-make-hook-value ,unless ,var))
		 (result
		  (iread-make-hook-value ,result ,var))
		 (iread-require-functions
		  (iread-make-hook-value ,require ,var))
		 (iread-delay ,delay)
		 (iread-min ,min)
		 (default ,default)
		 iread-ignore-prefix
		 iread-minibuffer-message
		 (minor-mode-map-alist
		  (cons (cons 'iread-mode (iread-prep-map ,keymap))
			minor-mode-map-alist)))
	     (unwind-protect
		 (iread-mode 1)
	       (iread-mode 0))
	     (if (and default (string-equal iread-input ""))
		 (setq iread-input default))
	     (if result (if (functionp result)
			    (funcall result iread-input)
			  result) iread-input)))))

(provide 'iread)

;;; iread.el ends here
