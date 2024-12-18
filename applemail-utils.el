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
(require 'f)
(require 'eieio)
(require 'cl-lib)

(defclass applemail/time ()
  ((date :initarg :date
	 :documentation "A list of the numerical day month and year")
   (original :initarg :original
	     :documentation "The original timestamp")
   (time :initarg :time
	 :documentation "A list of the second minute and hour")))

(cl-defmethod applemail-time/parse ((time applemail/time))
  "Parse the original part of TIME to date and time."
  (with-slots (original)
      time
    (cl-destructuring-bind (seconds minutes hours day month year dow dst utcoff)
	(parse-time-string original)
      (setf (slot-value time 'date) (list day month year))
      (setf (slot-value time 'time)  (list seconds minutes hours))
      time)))

(cl-defmethod applemail-time/show ((apple-time applemail/time))
  "Format the APPLE-TIME with y-m-dTH:M:s."
  (with-slots (date time)
      apple-time
    (when (and date time)
      (cl-destructuring-bind (day month year)
	  date
	(cl-destructuring-bind (seconds minutes hours)
	    time
	  (format "%s-%02d-%02dT%02d:%02d:%02d" year month day hours minutes seconds))))))

;; todo: we could avoid all this if we just used emacs time representation
(defun applemail-time--zip-lists (list-one list-two)
  "Make a list of pairs of items of LIST-ONE and LIST-TWO."
  (when (and list-one list-two)
      (cons (list (car list-one) (car list-two))
	    (applemail-time--zip-lists (cdr list-one) (cdr list-two)))))

(defun applemail-time--comparison (time-one time-two)
  "Compare TIME-ONE and TIME-TWO.
Return NIL if they are equal
Return :more if TIME-ONE is after TIME_TWO
Return :less if TIME-ONE is before TIME-TWO.

This is private function filled with horror -- look away and don't use."
  (let* ((date-one (slot-value time-one 'date))
	 (date-two (slot-value time-two 'date))
	 (time-one (slot-value time-one 'time))
	 (time-two (slot-value time-two 'time))
	 (time-grains-one (cl-concatenate 'list (reverse date-one) (reverse time-one)))
	 (time-grains-two (cl-concatenate 'list (reverse date-two) (reverse time-two))))
    (cl-some (lambda (grain-pair)
	       (cond ((< (car grain-pair) (cadr grain-pair))
		      :less)
		     ((> (car grain-pair) (cadr grain-pair))
		      :more)
		     (t nil)))
	     (applemail-time--zip-lists time-grains-one time-grains-two))))

(cl-defmethod applemail-time/earlier?
    ((apple-time-one applemail/time) (apple-time-two applemail/time))
  "Return T if APPLE-TIME-ONE happened before APPLE-TIME-TWO."
  (equal (applemail-time--comparison apple-time-one apple-time-two) :less))

(cl-defmethod applemail-time/later?
    ((apple-time-one applemail/time) (apple-time-two applemail/time))
  "Return T if APPLE-TIME-ONE happened after APPLE-TIME-TWO."
  (equal (applemail-time--comparison apple-time-one apple-time-two) :more))

(defclass applemail/message ()
  ((read :initarg :read
	 :documentaion "Whether or not the message has been read.")
   (received :initarg :received
	      :documentation "The time that the message was received")
   (sent :initarg :sent
	 :documentation "The time that the message was sent")
   (id :initarg :id
       :documentation "The id apple Mail associates with the message")
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
    (let ((file (format "/tmp/mail-%s" id)))
      (when (file-exists-p file)
	(setf (slot-value message 'content) (f-read-text file))
	message))))

(cl-defmethod applemail-message/mark-read ((message applemail/message))
  "Mark MESSAGE as read and tell that to Mail."
  (with-slots (id)
      message
    (apple/execute!
     (apple/tell-application
      "Mail"
      (apple/set
       (format "read status of every message of inbox whose id is %s" id) "true")))
    (setf (slot-value message 'read) t)
    message))

(cl-defmethod applemail-message/delete ((message applemail/message))
  "Delete MESSAGE from Mail."
  (with-slots (id)
      message
    (apple/execute!
     (apple/tell-application
      "Mail"
      (format "delete every message of inbox whose id is %s" id)))
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
    (applemail-time/earlier? time-one time-two)))

(defun applemail-message/sent-later? (message-one message-two)
  "Check if MESSAGE-ONE was sent earlier than MESSAGE-TWO."
  (let ((time-one (applemail-message/sent message-one))
	(time-two (applemail-message/sent message-two)))
    (applemail-time/later? time-one time-two)))

(defun applemail/new-message (id subject sender read received sent)
  "Create a message from ID, SUBJECT, SENDER, READ, RECEIVED, and SENT."
  (let ((sent-time (applemail-time/parse (applemail/time :original sent)))
	(rec-time (applemail-time/parse (applemail/time :original received)))
	(read-bool (equal (downcase read) "true")))
    (applemail/message :id id :subject subject :sender sender
		       :read read-bool :sent sent-time :received rec-time)))

(defun applemail--script->json-and-quit (script)
  "Run SCRIPT - convert its output to json, and quit mail when done."
  (let* ((raw-messages (apple/execute!
			script :message "fetching mail..." :timed? t))
	 (result (json-parse-string (thread-last raw-messages
						 string-trim
						 (format "[%s]")
						 (string-replace "\n" "\\n"))
				    :object-type 'plist)))
    (apple/execute! (apple/tell-application "Mail" "quit"))
    result))

(defun applemail--get-messages-json (limit offset)
  "Get the messages from inbox with LIMIT and OFFSET as JSON plists."
  (let ((message-restriction (format "messages %s through %s of inbox" offset limit)))
    (applemail--script->json-and-quit
     (apple/progn
      (apple/defun
       "escapeQuotes" (list "theText")
       (apple/set "theResult" "theText")
       (apple/set "astid" "AppleScript's text item delimiters")
       (apple/set "AppleScript's text item delimiters" "quote")
       (apple/set "theResult" "text items of theResult")
       (apple/set "AppleScript's text item delimiters" "\"\\\\\" & quote")
       (apple/set "theResult" "theResult as text")
       (apple/set "AppleScript's text item delimiters" "astid")
       (apple/return "theResult"))
      (apple/defun
       "writeTextToFile" (list "theText" "theFile")
       (apple/try
	(apple/progn
	 (apple/set "posixFile" "POSIX path of theFile")
	 (apple/set "fhandle" "open for access posixFile with write permission")
	 "write theText to fhandle as «class utf8»"
	 "close access fhandle"
	 (apple/return "true"))
	(apple/progn
	 (apple/try "close access posixFile")
	 (apple/return "false"))))
      (apple/tell-application
       "Mail"
       (apple/set "theResult" (apple/make-list))
       (apple/set "theMessages" message-restriction)
       (apple/dolist
	"theMessage" "theMessages"
	(apple/progn
	 (apple/set "theFilePath" "\"/tmp/mail-\" & id of theMessage")
	 (apple/set "theContent" "content of theMessage")
	 (apple/set
	  "end of theResult"
	  (apple/->json
	   '(("subject" .  "my escapeQuotes(subject of theMessage)")
	     ("id" . "id of theMessage")
	     ("sender". "my escapeQuotes(sender of theMessage)")
	     ("sent" . "date sent of theMessage")
	     ("read" . "read status of theMessage")
	     ("received" . "date received of theMessage"))))
	 "my writeTextToFile(theContent, theFilePath)"))
       (apple/return "theResult"))))))

(cl-defun applemail/get-messages (&key limit offset)
  "Get messages from inbox with LIMIT and OFFSET."
  (let* ((defaulted-offset (or offset 1))
	 (corrected-limit (+ (or limit 500) (- defaulted-offset 1))))
    (mapcar
     (lambda (message)
       (cl-destructuring-bind
	     (&key id subject sender read received sent &allow-other-keys)
	   message
	 (applemail/new-message id subject sender read received sent)))
     (applemail--get-messages-json corrected-limit defaulted-offset))))

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
