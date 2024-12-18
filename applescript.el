;;; applescript --- Summary
;;;
;;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;;; Version: 0.0.1
;;; Package-Requires: ((emacs "29.1") (f "0.20.0"))
;;; Keywords: email, macos
;;;
;;; Commentary:
;;;
;;; Write and run applescript in Emacs Lisp
;;;
;;; Code:
(require 'subr-x)
(require 'cl-macs)

(cl-defun apple/execute! (script &key message timed?)
  "Execute the applescript SCRIPT.
Optionally supply MESSAGE.
When TIMED? also display the time the execution took."
  (save-window-excursion
    (let* ((temporary-file (make-temp-file "applescript"))
	   (buffer (find-file temporary-file))
	   (execution-message message)
	   (start-time (float-time)))
      (let ((inhibit-message t))
	(switch-to-buffer buffer)
	(insert script)
	(save-buffer)
	(kill-buffer buffer))
      (when execution-message
	(message execution-message))
      (let ((result (shell-command-to-string
		     (format "osascript %s" temporary-file))))
	(when timed?
	  (message "applescript done. took %ss"
		   (- (float-time) start-time)))
        (kill-buffer buffer)
	result))))

(defun apple--indent (string)
  "Indent each line of STRING with four spaces."
  (let ((indent (make-string 4 ?\s)))
    (string-join
     (mapcar
      (lambda (line)
	(concat indent line))
      (split-string string "\n"))
     "\n")))

(defun apple--snoc (item lista)
  "Add ITEM to end of LISTA."
  (reverse (cons item (reverse lista))))

(defun apple/progn (&rest commands)
  "Do all of COMMANDS."
  (string-join commands "\n"))

(defun apple--if-else (condition do-if do-else)
  "Create an applescript string for to DO-IF on CONDITION else DO-ELSE."
  (string-join
   (list
    (format "if %s then" condition)
    (apple--indent do-if)
    "else"
    (apple--indent do-else)
    "end if")
   "\n"))

(defun apple/when (condition do-if)
  "Create an applescript string to DO-IF on CONDITION."
  (string-join
   (list
    (format "if %s then" condition)
    (apple--indent do-if)
    (format "end if"))
   "\n"))

(defun apple/if (condition do-if &optional do-else)
  "Create an applescript string to DO-IF on CONDITION.
Optionally specify DO-ELSE to do otherwise."
  (if do-else
      (apple--if-else condition do-if do-else)
    (apple/when condition do-if)))

(defun apple/defun (name args &rest body)
  "Create an applescript function with NAME, ARGS, and BODY."
  (string-join
   (thread-last body
		(mapcar #'apple--indent)
		(apple--snoc (format "end %s" name))
		(apple--snoc "")
		(apple--snoc "")
		(cons (format "on %s(%s)" name (string-join args ", "))))
   "\n"))

(defun apple/tell (spec &rest commands)
  "Create a tell block of SPEC with COMMANDS.
SPEC is a plist that contains at least `:type`, and `:item`.
`:type` a keyword, controls the type of tell, e.g. :application
`:item` a string, controls the item told, e.g. Mail"
  (if (or (not (plist-member spec :type))
	  (not (plist-member spec :item)))
      (warn "apple tell block spec %s missing :type or :item" spec)
    (let* ((item-type (plist-get spec :type))
	   (item (plist-get spec :item))
	   (item-type-string (substring (symbol-name item-type) 1)))
      (string-join
       (cons
	(format "tell %s \"%s\"" item-type-string item)
	(apple--snoc
	 "end tell"
	 (mapcar #'apple--indent commands)))
       "\n"))))

(defun apple/tell-application (name &rest commands)
  "Generate a tell block for type application with NAME with COMMANDS."
  (let ((spec (list :item name :type :application)))
    (apply #'apple/tell spec commands)))

(defun apple/try (to-try &optional on-error)
  "Create a try block with TO-TRY, optionally do something ON-ERROR."
  (let ((commands-reversed (list (apple--indent to-try) "try")))
    (when on-error
      (push "on error" commands-reversed)
      (push (apple--indent on-error) commands-reversed))
    (push "end try" commands-reversed)
    (apply #'apple/progn (reverse commands-reversed))))

(defun apple/make-properties (&rest properties)
  "Make a property list of PROPERTIES.
Assumes properties are passed in KEY-VALUE order."
  (let ((result "{"))
    (cl-do ((todo properties (cddr todo))
	    (key (car properties) (caddr todo))
	    (value (cadr properties) (cadddr todo)))
	   ((not todo) (format "%s}" result))
      (let ((new-pair (if (<= (length todo) 2)
			  (format "%s: %s" key value)
			(format "%s: %s, " key value))))
	(setq result (format "%s%s" result new-pair))))))

(defun apple/make-list (&rest items)
  "Make a list of ITEMS."
  (let ((result "{"))
    (cl-do ((todo items (cdr todo))
	    (item (car items) (cadr todo)))
	   ((not todo) (format "%s}" result))
      (let ((new-item (if (<= (length todo) 1)
			  (format "%s" item)
			(format "%s," item))))
	(setq result (format "%s%s" result new-item))))))

(defun apple/set (expression-one expression-two)
  "Set the EXPRESSION-ONE to the value of EXPRESSION-TWO."
  (format "set %s to %s" expression-one expression-two))

(defun apple/quote (string)
  "Quote STRING so it can be treated as a string in a script."
  (format "\"%s\"" string))

(defun apple/return (expression)
  "Have applescript return EXPRESSION."
  (format "return %s" expression))

(defun apple/dolist (item-var lista &rest commands)
  "Loop over items in LISTA binding ITEM-VAR as item."
  (string-join
   (cons
    (format "repeat with %s in %s" item-var lista)
    (apple--snoc
     "end repeat"
     (mapcar #'apple--indent commands)))
   "\n"))

(defun apple--json-key-value (key value &optional string-value?)
  "Create a json KEY VALUE pair.
Optional STRING-VALUE?: Specifies whether or not the passed item is a string or
an applescript variable."
  (let ((quote (unless string-value? "\\\"")))
    (format "\\\"%s\\\": %s\"& %s & \"%s"
	    key quote value quote)))

(defun apple/->json (alist)
  "Create apple json from ALIST."
  (let ((json-objects '()))
    (dolist (key (mapcar #'car alist))
      (let ((value (alist-get key alist nil nil #'equal)))
	(push (apple--json-key-value key value) json-objects)))
    (format "\"{%s}\"" (string-join json-objects ","))))

(provide 'applescript)
;;; applescript.el ends here
