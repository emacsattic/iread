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

;; Iread is Isearch without the search---a framework for acting
;; incrementally on user input.  It is implemented as a minor mode,
;; with a number of normal and abnormal hooks.  But all the moving
;; parts are encased by the function `iread', which lets you build up
;; incremental read functionality by passing keyword arguments.

;; To get a sense for how it works, try calling `iread-mode' directly.
;; A prompt appears in the minibuffer.  Type something and it appears
;; after the prompt.  Any event that is not a printable character
;; exits.  Iread is just a skeleton; but one with many points of
;; attachment.

;; Here are the parameters to `iread' and the variables they bind.

;; PROMPT, a required argument, binds `iread-prompt'.  This is the
;; prompt that `iread-mode' displays.

;; VAR, a required argument, specifies a symbol to use as an argument
;; when constructing functions.  If VAR is nil, you can pass only
;; functions and lists of functions to a parameter that binds an
;; abnormal hook.  But if VAR is a symbol, you can also pass a quoted
;; form to use as the body of a function.  The quoted form is wrapped
;; in a function with VAR as its single argument.
;; For example,
;;    (iread "Don't: " 'var :do '(ignore var))
;; Is equivalent to:
;;    (iread "Don't: " nil :do (lambda (var) (ignore var)))

;; LIGHTER binds `iread-lighter'.  This is the string to display in
;; the modeline while `iread-mode' is on.

;; INPUT binds `iread-input'---an initial input.

;; DEFAULT is a local binding.  If the user does not type anything,
;; this is the value to return.

;; DO binds `iread-after-input-functions', an abnormal hook.  The
;; functions in this hook are called with one argument, the
;; current value of `iread-input'.

;; KEYMAP is a keymap containing additional keybindings to use while
;; `iread-mode' is on.  You can provide either a keymap as such, or as
;; a list in the format that `easy-mmode-define-keymap' expects.

;; (The keymap is copied, and the copy has its parent set to
;; `iread-mode-map'.)

;; FIRST binds `iread-mode-on-hook', a normal hook to run
;; when `iread-mode' turns on.

;; UNTIL binds `iread-end-test-functions', an abnormal hook.  The
;; functions in this hook are called with the current value of
;; `iread-input'.  If any function returns non-nil then `iread-mode'
;; is turned off and `iread' peremptorily returns.

;; UNLESS binds `iread-fail-test-functions', another abnormal hook.
;; When a function in this hook returns non-nil, it prevents
;; `iread-after-input-functions' from being run.  It also sets
;; `iread-fail-prefix', so further input (input for which
;; `iread-fail-prefix' satisfied `string-prefix-p') can be disregarded
;; without testing.

;; DELAY binds `iread-delay'.  This is the number of seconds to wait
;; after input before running `iread-after-input-functions'.

;; MIN binds `iread-delay'.  `iread-after-input-functions' will not be
;; run unless `iread-input' is longer than MIN.  Note that the default
;; value is 0, meaning that nothing is done until something is
;; actually typed; if you want `iread-after-input-functions' to be run
;; as soon as `iread-mode' starts---or if the user deletes all
;; input---then you must set MIN to -1.

;; LAST binds `iread-mode-off-hook', a normal hook run when
;; `iread-mode' turns off.

;; RETURN is a local binding.  If a function is passed as RETURN then
;; this function will be called with `iread-input' and its return
;; value will be the return value of `iread'.  For example,
;;    (iread "Characters: " nil :return 'string-to-list)
;; Is equivalent to:
;;    (string-to-list (iread "Characters: " nil))

;; N.  B.  The call to `iread-mode' is wrapped with the tag 'iread.
;; You can throw to this tag at any time; `iread-input' will be set to
;; the value of the throw.  This does not exit `iread' itself; LAST
;; and RETURN will still be run.

;;; Code:

(eval-when-compile (require 'cl))

(defvar iread-recursive nil
  "Whether Iread should act modally.")

(defvar iread-lighter ""
  "Lighter to display in mode line while reading.")
(defvar iread-prompt "Type: ")
(defvar iread-input "")
(defvar iread-echo "")
(defvar iread-fail-prefix nil)
(defvar iread-delay 0)
(defvar iread-min 0)

(defvar iread-mode-hook nil)
(defvar iread-mode-on-hook nil)
(defvar iread-mode-off-hook nil)
(defvar iread-after-input-functions nil)
(defvar iread-end-test-functions nil)
(defvar iread-fail-test-functions nil)

(defsubst iread-input ()
  "Return variable `iread-input' as a list (for `interactive')."
  (if iread-mode
      (list iread-input)))

(defun iread-build-prompt ()
  "Build minibuffer contents for function `iread-mode'."
  (concat (propertize iread-prompt
		      'face 'minibuffer-prompt)
	  iread-input
	  iread-echo))

(defun iread-backspace ()
  "Delete the last input character."
  (interactive)
  (setq iread-input
	(or
	 (condition-case nil
	     (substring iread-input 0 -1)
	   (args-out-of-range nil))
	 iread-input))
  (iread-do)
  (iread-update))

(defun iread-fail-test ()
  "Do nothing if input fails."
  (or
   (and iread-fail-prefix
	(string-prefix-p iread-fail-prefix iread-input))
   (and (run-hook-with-args-until-success
	 'iread-fail-test-functions iread-input)
	(setq iread-fail-prefix iread-input))
   (setq iread-fail-prefix nil)))

(defun iread-do ()
  "Run `iread-after-input-functions' if appropriate."
  (and (> (length iread-input) iread-min)
       (let ((message-log-max nil))
	 ;; This prevents flickering.
	 (with-temp-message (iread-build-prompt)
	   (sit-for iread-delay t)))
       (not (iread-fail-test))
       (run-hook-with-args 'iread-after-input-functions iread-input))
  (iread-update))

(defun iread-append ()
  "Append to input."
  (interactive)
  (setq iread-input
	(concat iread-input (char-to-string last-command-event)))
  (iread-update)
  (iread-do))

(defun iread-down-event-p (ev)
  "Discard EV if it is a down event."
  (if (mouse-event-p ev)
      (memq 'down (event-modifiers ev))))

(defun iread-fall-through ()
  "Turn iread off and continue command."
  (interactive)
  (let ((ev last-command-event))
    ;; We ignore the down event so the up event will have a chance at
    ;; the keymap.  (If the down event itself is bound we will never
    ;; get here).
    (if (iread-down-event-p ev)
	(setq this-command 'undefined)
      (progn
	(push ev unread-command-events)
	(iread-mode 0)))))

(defun iread-edit ()
  "Edit input non-incrementally."
  (interactive)
  (with-local-quit
   (let ((overriding-terminal-local-map minibuffer-local-map))
     (setq iread-input
	   (minibuffer-with-setup-hook (lambda () (insert iread-input))
	     (read-string (concat iread-prompt "[EDIT] "))))))
  (iread-update)
  (iread-do))

(defun iread-echo (string args)
  "Pass STRING and ARGS to `format' and show after minibuffer contents."
  (interactive)
  (let ((str (apply 'format (list* format-string args nil))))
   (setq iread-echo str)
   (iread-update)
   str))

(defun iread-update ()
  "Update minibuffer display."
  (if (run-hook-with-args-until-success
       'iread-end-test-functions
       iread-input)
      (iread-mode 0)
    (let ((message-log-max nil))
      (message (format "%s" (iread-build-prompt))))))

(defun iread-define-keymap (map)
  "Make a keymap from MAP."
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
If MAP is non-nil, return a copy of it.
If MAP is nil, return `iread-mode-map'."
  (if map
   (iread-adopt-map
    (iread-define-keymap map))
   iread-mode-map))

(defun iread-make-function (form &optional var)
  "If needed, wrap FORM in a function taking VAR."
  (cond
   ((functionp form)
    form)
   ((loop for fun in form always (functionp fun))
    form)
   ((and var (listp form))
    `(lambda (,var) ,form))
   ((and (null var) (listp form))
    `(lambda nil ,form))
   (t (error "No function"))))

(defun iread-quit ()
  "Exit Iread."
  (interactive)
  (if iread-recursive
      (progn
       (setq iread-input nil)
       (ignore-errors (exit-recursive-edit))
       (call-interactively 'keyboard-quit))
    (call-interactively 'keyboard-quit)))

(defvar iread-mode-map
  (let ((map (make-keymap)))
    ;; Bind all printable chars to 'iread-append.
    (map-char-table
     (lambda (k v)
       (set-char-table-range (nth 1 map) k 'iread-append))
     printable-chars)
    ;; Everything not explicitly bound should quit.
    (define-key map [t] 'iread-fall-through)
    (define-key map "\C-e" 'iread-edit)
    (define-key map "\C-g" 'iread-quit)
    (define-key map "\C-h" 'help-command)
    (define-key map "\C-u" 'universal-argument)
    (define-key map "\r" 'iread-mode)
    (define-key map "\d" 'iread-backspace)
    map)
  "Current keymap for `iread-mode'.")

;;;###autoload
(define-minor-mode iread-mode
  "Incremental read mode."
  :init-value nil
  :global t
  :lighter iread-lighter
  :keymap iread-mode-map
  (if iread-mode
      (progn
	(iread-update)
	(if iread-recursive (recursive-edit)))
    (ignore-errors (exit-recursive-edit))))

;;;###autoload
(defun* iread (prompt
	       var
	       &key
	       lighter
	       input
	       default
	       do
	       keymap
	       first
	       until
	       unless
	       last
	       return
	       delay
	       min)
  (let ((iread-prompt prompt)
	(iread-lighter lighter)
	(iread-input input)
	(iread-echo nil)
	(iread-after-input-functions (iread-make-function do var))
	(iread-mode-on-hook (iread-make-function first))
	(iread-end-test-functions (iread-make-function until var))
	(iread-fail-prefix nil)
	(iread-fail-test-functions (iread-make-function unless var))
	(iread-mode-off-hook (iread-make-function last))
	(iread-delay (or delay 0))
	(iread-min (or min 0))
	(minor-mode-map-alist
	 (cons (cons 'iread-mode (iread-prep-map keymap))
	       minor-mode-map-alist))
	(iread-recursive t))
    (unwind-protect
     (setq iread-input
	   (catch 'iread
	     (iread-mode 1)
	     iread-input))
     (iread-mode 0))
    (if (and default
	     (string-equal iread-input ""))
	(setq iread-input default))
    (if return (funcall return iread-input) iread-input)))
(put 'iread 'lisp-indent-hook 2)

(provide 'iread)

;;; iread.el ends here
