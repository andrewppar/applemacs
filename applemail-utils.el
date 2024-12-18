;;; applemail-utils --- Summary
;;;
;;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;;; Version: 0.0.1
;;; Package-Requires: ((emacs "29.1") (f "0.20.0"))
;;; Keywords: email, macos
;;;
;;; Commentary:
;;;
;;; CRUD operations on applemail from Emacs
;;;
;;; Code:
(require 'applescript)
(require 'apple-db)
(require 'apple-time)
(require 'f)
(require 'eieio)
(require 'cl)

(defclass applemail/message ()
  ((read :initarg :read
	 :documentaion "Whether or not the message has been read.")
   (receieved :initarg :received
	      :documentation "The time that the message was received")
   (sent :initarg :sent
	 :documentation "The time that the message was sent")
   (id :initarg :id
       :documentation "The id apple Mail associates with the message")
   (mailbox :initarg :mailbox
	    :documentation "The mailbox URI of the message.")
   (subject :initarg :subject
	    :documentation "The subject of the message")
   (sender :initarg :sender
	   :documentation "The sender of the message")
   (content :documentation "The content of the message")))

(defmacro declare-applemail-accessor (slot-value)
  "Create an accessor named applemail-message/SLOT-VALUE for messages."
  (let ((method-name (make-symbol (format "applemail-message/%s" slot-value))))
    `(cl-defmethod ,method-name ((message applemail/message))
       ,(format "Get the value of %s for MESSAGE." method-name)
       (slot-value message ',slot-value))))

(cl-defmethod applemail-message/read ((message applemail/message))
  "Get the value of applemail-message/read for MESSAGE."
  (slot-value message 'read))

(cl-defmethod applemail-message/received
    ((message applemail/message))
  "Get the value of applemail-message/received for MESSAGE."
  (slot-value message 'received))

(cl-defmethod applemail-message/sent
    ((message applemail/message))
  "Get the value of applemail-message/sent for MESSAGE."
  (slot-value message 'sent))

(cl-defmethod applemail-message/id
    ((message applemail/message))
  "Get the value of applemail-message/id for MESSAGE."
  (slot-value message 'id))

(cl-defmethod applemail-message/subject
    ((message applemail/message))
  "Get the value of applemail-message/subject for MESSAGE."
  (slot-value message 'subject))

(cl-defmethod applemail-message/sender ((message applemail/message))
  "Get the value of applemail-message/sender for MESSAGE."
  (slot-value message 'sender))

(cl-defmethod applemail-message/content
    ((message applemail/message))
  "Get the value of applemail-message/content for MESSAGE."
  (slot-value message 'content))

(cl-defmethod applemail-message/fetch-content ((message applemail/message))
  "Get the content of MESSAGE."
  (with-slots (id)
      message
    (let ((content (apple/execute!
		    (apple/tell-application
		     "Mail"
		     (apple/return
		      (format "content of first message of inbox whose id is %s" id)))
		    :message "fetching message..."
		    :timed? t)))
      (setf (slot-value message 'content) content)
      message)))

(cl-defmethod applemail-message/mark-read ((message applemail/message))
  "Mark MESSAGE as read and tell that to Mail."
  (with-slots (id)
      message
    (apple/execute!
     (apple/tell-application
      "Mail"
      (apple/set
       (format "read status of every message of inbox whose id is %s" id) "true")))
    (set-slot-value message 'read t)
    message))

(cl-defmethod applemail-message/open ((message applemail/message))
  "Open MESSAGE in Mail."
  (with-slots (id)
      message
    (apple/execute!
     (apple/tell-application
      "Mail"
      (apple/set "messageID" id)
      "open first message of inbox whose id is messageID"))
    nil))

(defun applemail-message/sent-earlier? (message-one message-two)
  "Check if MESSAGE-ONE was sent earlier than MESSAGE-TWO."
  (let ((time-one (applemail-message/sent message-one))
	(time-two (applemail-message/sent message-two)))
    (apple-time/earlier-p time-one time-two)))

(defun applemail-message/sent-later? (message-one message-two)
  "Check if MESSAGE-ONE was sent earlier than MESSAGE-TWO."
  (let ((time-one (applemail-message/sent message-one))
	(time-two (applemail-message/sent message-two)))
    (apple-time/later-p time-one time-two)))

(defun applemail/new-message (id subject sender read received sent)
  "Create a message from ID, SUBJECT, SENDER, READ, RECEIVED, and SENT."
  (let ((sent-time (apple-time/appletime->emacs sent))
	(rec-time (apple-time/appletime->emacs received))
	(read-bool (equal (downcase read) "true")))
    (applemail/message :id (format "%s" id)
		       :subject subject
		       :sender sender
		       :read read-bool
		       :sent sent-time
		       :received rec-time)))

(defun applemail--get-messages-json (limit offset unread-only? mailbox)
  "Get messages from INBOX.
LIMIT: the max number of messages to return (defaults to 144)
OFFSET: an offset to start from
UNREAD-ONLY?: whether or not to include read messages
MAILBOX: the name of a mailbox to limit the search to"
  (let ((limit (or limit 100))
	(read-clause (when unread-only? "read = 0"))
	(mailbox-clause (when mailbox (concat "mailboxes.url like '%" mailbox "%'"))))
    (apple-db/execute!
     *apple-db/mail-db*
     (apple-db/query
      (apple-db/select
       (list
	(apple-db/column "messages" "ROWID" :as "id")
	(apple-db/column "messages" "read" :as "read")
	(apple-db/column "messages" "date_received" :as "received")
	(apple-db/column "messages" "date_sent" :as "sent")
	(apple-db/column "addresses" "address" :as "sender")
	(apple-db/column "subjects" "subject" :as "subject")
	(apple-db/column "mailboxes" "url" :as "mailbox")))
      (apple-db/from "messages")
      (apple-db/join "addresses" :on "messages.sender = addresses.ROWID")
      (apple-db/join "subjects" :on "messages.subject = subjects.ROWID")
      (apple-db/join "mailboxes" :on "messages.mailbox = mailboxes.ROWID")
      (apple-db/where (string-join
		       (remove nil (list "deleted = 0" read-clause mailbox-clause))
		       " and "))
      (apple-db/order-by (list "date_received" :desc))
      (apple-db/limit :limit limit :offset offset)))))

(cl-defun applemail/get-messages
    (&key limit offset mailbox unread-only?)
  "Get messages from inbox with LIMIT and OFFSET."
  (let* ((defaulted-offset (or offset 1))
	 (corrected-limit (+ (or limit 500) (- defaulted-offset 1))))
    (mapcar
     (lambda (message)
       (cl-destructuring-bind
	     (&key id subject sender read received sent mailbox &allow-other-keys)
	   message
	 (applemail/new-message
	  id
	  subject
	  sender
	  (if (= read 1) "true" "false")
	  (decode-time received)
	  (decode-time sent))))
     (applemail--get-messages-json
      corrected-limit defaulted-offset unread-only? mailbox))))

(cl-defun applemail/search-messages (search-term &key search-field limit)
  "Return any messages whose subject matches SEARCH-TERM.
Pass LIMIT to search the first LIMIT messages of inbox.
Pass SEARCH-FIELD to specify what part of the message to search,
this can be one of :subject (default), :sender, or :content."
  (let ((valid-fields '(:subject :sender :content))
	(field nil)
	(search-limit nil))
    (if (not search-field)
	(setq field :subject)
      (unless (member search-field valid-fields)
	(error (format ":search-field must be one of %s" valid-fields)))
      (setq field search-field))
    (let* ((max-message-limit (thread-last
				(apple/return "count of messages of inbox")
				(apple/tell-application "Mail")
				apple/execute!
				string-trim
				string-to-number)))
      (setq search-limit (or (and limit (min limit max-message-limit))
			     max-message-limit)))
    (seq-filter
     (lambda (message)
       (let* ((search-field-symbol (thread-first search-field
						 symbol-name
						 (substring 1)
						 make-symbol)))
	 (thread-last search-field-symbol
		      (slot-value message)
		      (string-match-p (regexp-quote search-term)))))
     (applemail/get-messages :limit search-limit))))

(provide 'applemail-utils)
;;; applemail-utils.el ends here
