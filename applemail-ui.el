;;; applemail-ui --- Summary
;;;
;;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;;; Version: 0.0.1
;;; Package-Requires: ((emacs "29.1") (f "0.20.0"))
;;; Keywords: email, macos
;;;
;;; Commentary:
;;;
;;; UI to manage you apple mail
;;;
;;; Code:
(require 'applemail-utils)
(require 'eieio)

;;;###autoload
(defvar *applemail/inbox*
  nil
  "Inbox to use - can be a sql regex, e.g. =all%mail=")

(defconst *applemail-inbox-buffer* "*applemail - INBOX*")

(defun applemail-ui--count-unread-messages (messages)
  "Get the unread count of messages in MESSAGES."
  (let ((result 0))
    (dolist (message messages)
      (when (applemail-message/read message)
	(setq result (+ result 1))))
    result))

(defclass applemail-ui/inbox-messages ()
  ((messages :initarg :messages
	     :type list
	     :initform '()
	     :documentation "The current messages of inbox.")
   (unread-count :type number
		 :initform 0)
   (field->max-value-size :type list
			  :initform '())
   (selected-message :type applemail/message
		     :documentation "The selected message of the inbox.")
   (marked-messages :initarg :marked-messages
		    :type list
		    :initform '()
		    :documentation "Currently marked messages.")
   (max-size :initarg :maxsize
	     :type number
	     :initform 500
	     :documentation "The maximum size of inbox in memory.")))

(cl-defmethod initialize-instance :after ((obj applemail-ui/inbox-messages) &rest _args)
  (with-slots (messages) obj
      (setf (slot-value obj 'unread-count)
	    (applemail-ui--count-unread-messages messages))
      obj))

(cl-defmethod applemail-ui-inbox-messages/push
    ((message applemail/message)
     (inbox-messages applemail-ui/inbox-messages))
  "Add MESSAGE to INBOX-MESSAGES."
  (with-slots (max-size messages field->max-value-size)
      inbox-messages
    (with-slots (id)
	message
      (unless (member id (mapcar #'applemail-message/id messages))
	(when (< max-size (length messages))
	  (setq messages (reverse (cdr (reverse messages)))))
	(push message messages)
	(dolist (field '(id sent subject sender))
	  (let* ((in-slot (slot-value message field))
		 (value (if (equal field 'sent)
			    (length (format-time-string
				     "%Y-%m-%dT%H:%M:%s"
				     (encode-time in-slot)))
			  (length in-slot))))
	    (when (>= value (alist-get field field->max-value-size 0))
	      (setf (alist-get field field->max-value-size) value)))))))
  inbox-messages)

(cl-defmethod applemail-ui-inbox-messages/push-marked-message
    ((message applemail/message)
     (inbox-messages applemail-ui/inbox-messages))
  "Mark a message."
  (with-slots (marked-messages) inbox-messages
    (with-slots (id) message
      (unless (member id (mapcar #'applemail-message/id marked-messages))
	(push message marked-messages))))
  inbox-messages)

(cl-defmethod applemail-ui-inbox-messages/remove-marked-message
    ((message applemail/message)
     (inbox-messages applemail-ui/inbox-messages))
  "Unmark a message."
  (with-slots (marked-messages) inbox-messages
    (with-slots (id) message
      (setf (slot-value inbox-messages 'marked-messages)
	    (seq-filter
	     (lambda (marked-message)
	       (not (equal id (applemail-message/id marked-message))))
	     marked-messages))))
  inbox-messages)

(cl-defmethod applemail-ui-inbox-messages/delete-marked
    ((inbox-messages applemail-ui/inbox-messages))
  (with-slots (marked-messages messages) inbox-messages
    (mapc #'applemail-message/delete marked-messages)
    (let* ((marked-ids (mapcar #'applemail-message/id marked-messages))
	   (kept-messages (seq-filter
			   (lambda (message)
			     (with-slots (id) message
			       (not (member id marked-ids))))
			   messages)))
      (setf (slot-value inbox-messages 'messages) kept-messages)
      (setf (slot-value inbox-messages 'marked-messages) '())))
  inbox-messages)

(cl-defmethod applemail-ui-inbox-messages/push-all
    (messages (inbox-messages applemail-ui/inbox-messages))
  "Add all MESSAGES to INBOX-MESSAGES."
  (dolist (message messages)
    (applemail-ui-inbox-messages/push message inbox-messages))
  inbox-messages)

(cl-defmethod applemail-ui-inbox-messages/selected-message
    ((inbox-messages applemail-ui/inbox-messages))
  "Get the selected message for INBOX-MESSAGES."
  (slot-value inbox-messages 'selected-message))

(cl-defmethod applemail-ui-inbox-messages/set-selected-message
    (message (inbox-messages applemail-ui/inbox-messages))
  "Set the selected message of INBOX-MESSAGES to MESSAGE."
  (setf (slot-value inbox-messages 'selected-message) message)
  inbox-messages)

(cl-defmethod applemail-ui-inbox-messages/get-message-by-id
    (id (inbox-messages applemail-ui/inbox-messages))
  "Get a message from INBOX-MESSAGES by its ID."
  (with-slots (messages)
      inbox-messages
    (cl-some
     (lambda (message)
       (when (equal id (slot-value message 'id))
	 message))
     messages)))

(cl-defmethod applemail-ui-inbox-messages/sort
    ((inbox-messages applemail-ui/inbox-messages) sort-fn)
  "Sort messages in INBOX-MESSAGES by SORT-FN."
  (with-slots (messages)
      inbox-messages
    (sort messages sort-fn)
    inbox-messages))

(cl-defmethod applemail-ui-inbox-messages/fetch-new
    ;; todo: optionally specify an amount
    ;; todo: assumes all fetches have been startng from offset 0 without gaps
    ((inbox-messages applemail-ui/inbox-messages))
  "Fetch the next 20 messages for INBOX-MESSAGES."
  (with-slots (messages)
      inbox-messages
    (let ((current-offset (max (- (length messages) 1) 0)))
      (applemail-ui-inbox-messages/push-all
       (applemail/get-messages
	:offset current-offset
	:limit 50
	:mailbox *applemail/inbox*)
       inbox-messages))
    inbox-messages))

(defvar *applemail-ui/inbox-messages* (applemail-ui/inbox-messages)
  "The current messages in the inbox.")

(define-minor-mode applemail-inbox-mode
    "The minor mode for navigating the applemail inbox.")

(defun applemail-ui--message-at-point ()
  "Get the id of the message at point."
  ;; todo: if this fails, we may have a state mismatch...
  (when-let ((message-row (save-excursion
			    (beginning-of-line)
			    (let ((start (point)))
			      (end-of-line)
			      (buffer-substring start (point))))))
    (let ((id (car (split-string message-row "|"))))
      (when (and id ;;(= (length id) 5)))
		 )
	(applemail-ui-inbox-messages/get-message-by-id
	 id *applemail-ui/inbox-messages*)))))

(defun applemail-ui--goto-message (message)
  "Search for MESSAGE in inbox buffer and put point on it."
  (goto-char (point-min))
  (let ((found-point nil))
    (while (not found-point)
      (if (eobp)
	  (progn
	    (setq found-point (point-max))
	    (goto-char (point-min)))
	(if-let ((message-at-point (applemail-ui--message-at-point)))
	    (with-slots (id)
		message-at-point
	      (if (equal id (applemail-message/id message))
		  (setq found-point (point))
		(forward-line)))
	  (forward-line))))
    (goto-char found-point)
    found-point))

(defmacro with-inhibit-read-only (&rest body)
  "Inhibit read-only for BODY."
  (declare (indent 0))
  `(let ((inhibit-read-only t)) ,@body))

;; maybe this should be in the mode line
(defun applemail-ui--insert-inbox-info (messages-to-show)
  (let* ((title (applemail-ui--color-text "APPLEMACS INBOX" "light blue"))
	 (unread-count (length (seq-filter
				(lambda (message)
				  (not (applemail-message/read message)))
				messages-to-show))))
    (insert
     (string-join
      (list title
	    (format "Unread Messages: %s" unread-count)
	    (format "Shown MessageS: %s" (length messages-to-show))
	    ""
	    "")
      "\n"))))

(defun applemail-ui--insert-inbox-header ()
  "Insert the inbox header for UI."
  (let ((maxes '((subject . 100) (sender . 60) (sent . 100000)))
	(header-list '("read" "id   ")))
    (with-slots (field->max-value-size messages)
	*applemail-ui/inbox-messages*
      (dolist (column '(subject sender sent))
	(let ((col-max (min (alist-get column maxes)
			    (alist-get column field->max-value-size)))
	      (col-string (format "%s" column)))
	  (push
	   (format "%s%s" col-string
		   (make-string (- (- col-max (length col-string)) 1) ?\ ))
	   header-list)))
      (let* ((header (format "%s" (string-join (reverse header-list) "| ")))
	     (div (applemail-ui--color-text (make-string (length header) ?=) "blue")))
	(insert (string-join (list header div "") "\n"))))))

(defun applemail-ui--color-text (text color)
  "Add COLOR to TEXT."
  (propertize text 'font-lock-face (list :foreground color)))

(defun applemail-ui--format-for-max (text max)
  "Format TEXT appropriately for MAX number of characters."
  (let ((text-size (length text)))
    (if (>= text-size max)
	(format "%s..." (substring text 0 (- max 3)))
      (let ((padding (make-string (- max text-size) ?\ )))
	(format "%s%s" text padding)))))

(defun applemail-ui--prep-message-to-insert (message)
  "Prep MESSAGE to insert into the inbox."
  ;; *applemail-ui/inbox-messages* this should be passed at some point
  (with-slots (field->max-value-size marked-messages) *applemail-ui/inbox-messages*
    (with-slots (id read sent subject sender) message
      (let* ((marked-status (if (member id (mapcar #'applemail-message/id marked-messages)) "M" " "))
	     (read-status (applemail-ui--color-text "·" (if read "green" "orange")))
	     (message-status (format "%s%s    " read-status marked-status))
	     (subject-text (applemail-ui--format-for-max
			    subject (min (alist-get 'subject field->max-value-size) 100)))
	     (sent-text (format-time-string "%Y-%m-%dT%H:%M:%s" (encode-time sent)))
	     (sender-text (applemail-ui--format-for-max
			   sender (min (alist-get 'sender field->max-value-size) 60)))
	     (row (list (format "%s" id) message-status subject-text sender-text sent-text)))
	(format "%s\n" (string-join  row "|"))))))

(defmacro save-inbox-excursion (&rest body)
  "Execute BODY but return to the message under point."
  (declare (indent 0))
  (let ((message (gensym)))
    `(let ((,message (applemail-ui--message-at-point)))
       (progn ,@body)
       (if ,message
	   (progn
	     (applemail-ui-inbox-messages/set-selected-message ,message *applemail-ui/inbox-messages*)
	     (applemail-ui--goto-message
	      (applemail-ui-inbox-messages/selected-message
	       *applemail-ui/inbox-messages*)))
	 (goto-char (point-min))))))

(defun applemail-display/refresh-inbox (&optional fetch-new? filter-fn)
  "Show the first 20 messages of the current inbox.
Optionally fetch new ones with FETCH-NEW?"
  (interactive)
  (let ((filter-function (or filter-fn #'identity)))
    (delete-other-windows)
    (switch-to-buffer *applemail-inbox-buffer*)
    (save-inbox-excursion
      (setq buffer-read-only t)
      (with-inhibit-read-only
	(hl-line-mode)
	(applemail-inbox-mode)
	(kill-region (point-min) (point-max))
	(when fetch-new?
	  (applemail-ui-inbox-messages/fetch-new *applemail-ui/inbox-messages*))
	(with-slots (messages)
	    (applemail-ui-inbox-messages/sort
	     *applemail-ui/inbox-messages* #'applemail-message/sent-later?)
	  (let ((shown-messages (seq-filter filter-function messages)))
	    (applemail-ui--insert-inbox-info shown-messages)
	    (applemail-ui--insert-inbox-header)
	    (dolist (message shown-messages)
	      (insert (applemail-ui--prep-message-to-insert message)))))))))

(defconst *applemail-message-buffer* "*applemail - MESSAGE*")

(define-minor-mode applemail-message-mode
    "The minor mode for navigating applemail message content.")

(defun applemail-ui--kill-buffer-safe (buffer-or-name)
  "Kill BUFFER-OR-NAME if it exists."
  (when-let ((buffer (get-buffer buffer-or-name)))
    (kill-buffer buffer)))

(defun applemail-display/inbox-quit ()
  "Quit Applemail."
  (interactive)
  (setq *applemail-ui/inbox-messages* (applemail-ui/inbox-messages))
  (mapcar #'applemail-ui--kill-buffer-safe
	  (list *applemail-message-buffer* *applemail-inbox-buffer*)))

(defun applemail-display/message-close ()
  "Close a the current message."
  (interactive)
  (when-let ((window (get-buffer-window *applemail-message-buffer*)))
    (delete-window window))
  (kill-buffer *applemail-message-buffer*)
  (switch-to-buffer *applemail-inbox-buffer*)
  (applemail-ui--goto-message
   (applemail-ui-inbox-messages/selected-message
    *applemail-ui/inbox-messages*)))

(defun applemail-display/delete-marked ()
  "Delete any marked messages."
  (interactive)
  (when (y-or-n-p "Are you sure you want to delete marked messages?")
    (applemail-ui-inbox-messages/delete-marked *applemail-ui/inbox-messages*)
    (applemail-display/refresh-inbox nil)))

(defun applemail-display/open-message ()
  "View the message at point in a dedicated buffer."
  (interactive)
  (when-let ((message (applemail-ui--message-at-point)))
    ;; todo: size this
    (split-window-below)
    (other-window 1)
    (switch-to-buffer *applemail-message-buffer*)
    (applemail-message-mode)
    (setq buffer-read-only t)
    (with-inhibit-read-only
      (kill-region (point-min) (point-max))
      (applemail-message/fetch-content message)
      (let ((header (thread-first
		      message
		      applemail-ui--prep-message-to-insert
		      (split-string "|")
		      cddr
		      (string-join "|"))))

	(insert header)
	(insert (applemail-ui--color-text
		 (make-string (length header) ?=)
		 "blue"))
	(insert "\n")
	(insert (slot-value message 'content))
	(applemail-message/mark-read message)
	(goto-char (point-min))
	(forward-line 2)))
    (save-window-excursion
      (switch-to-buffer *applemail-inbox-buffer*)
      (applemail-display/refresh-inbox))))

(defun applemail-ui--mark-message-row (unmark?)
  (let* ((line-start (save-excursion (beginning-of-line) (point)))
	 (line-end (save-excursion (end-of-line) (point)))
	 (fields (split-string (buffer-substring line-start line-end) "|"))
	    ;; 77 = M 32 = " "
	 (mark-char (if unmark? 32 77)))
    (when-let ((status (cadr fields)))
      (aset status 1 mark-char)
      (save-excursion
	(with-inhibit-read-only
	  (goto-char line-start)
	  (kill-region line-start line-end)
	  (insert (string-join fields "|")))))))

(defun applemail-display/mark-message ()
  (interactive)
  (when-let ((message (applemail-ui--message-at-point)))
    (applemail-ui-inbox-messages/push-marked-message
     message *applemail-ui/inbox-messages*)
    (applemail-ui--mark-message-row nil)
    (forward-line)))

(defun applemail-display/unmark-message ()
  (interactive)
  (when-let ((message (applemail-ui--message-at-point)))
    (applemail-ui-inbox-messages/remove-marked-message
     message *applemail-ui/inbox-messages*)
    (applemail-ui--mark-message-row t)
    (forward-line)))

(defun applemail-display/filter-unread ()
  (interactive)
  (applemail-display/refresh-inbox
   nil (lambda (message) (not (applemail-message/read message)))))

(defun applemail-display/open-in-mail ()
  "View the messsage at point in the Mail app."
  (interactive)
  (let ((current-buffer-name (buffer-name (current-buffer)))
	(message nil))
    (cond ((equal current-buffer-name *applemail-inbox-buffer*)
	   (setq message (applemail-ui--message-at-point)))
	  ((equal current-buffer-name *applemail-message-buffer*)
	   (setq message (applemail-ui-inbox-messages/selected-message
			   *applemail-ui/inbox-messages*)))
	  (t nil))
    (when message
      (applemail-message/open message))))

(evil-define-minor-mode-key 'normal 'applemail-inbox-mode
  (kbd "RET") 'applemail-display/open-message
  "D" 'applemail-display/delete-marked
  "f" (lambda () (interactive) (applemail-display/refresh-inbox t))
  "U" 'applemail-display/filter-unread
  "m" 'applemail-display/mark-message
  "o" 'applemail-display/open-in-mail
  "q" 'applemail-display/inbox-quit
  "r" 'applemail-display/refresh-inbox
  "u" 'applemail-display/unmark-message)

(evil-define-minor-mode-key 'normal 'applemail-message-mode
  "q" 'applemail-display/message-close
  "o" 'applemail-display/open-in-mail)

(provide 'applemail-ui)
;;; applemail-ui.el ends here
