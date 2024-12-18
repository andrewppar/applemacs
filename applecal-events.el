;; applecal-events --- Summary
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
(require 'subr-x)
(require 'applecal-utils)
(require 'apple-time)
(require 'applescript)

;;; TODO: Custom specifier rules to override
;;; e.g. monthly engineering meeting should be D=-1FR instead of D=+5FR
;;; this is borked in ical too, so I don't doubt my interpretation of the
;;; specifier

;;;###autoload
(defvar *applecal-events/calendars*
  ()
  "Calendars that are relevent to you.")

;;;###autoload
(defvar *applecal-events/custom-filter*
  (lambda (_x) t)
  "Any custom filtering to do on the raw events from the database.")

;;;###autoload
(defvar *applecal-events/custom-transform*
  #'identity
  "Custom tranformation to be appled to events before they are returned.")

(defun applecal-events--convert-times (events)
  "Convert time fields in EVENTS to Emacs times."
  (mapcar
   (lambda (entry)
     (cl-destructuring-bind (&key end_date start_date rend_date &allow-other-keys)
	 entry
       (thread-first
	 entry
	 (plist-put :end_date (apple-time/appletime->emacs end_date))
	 (plist-put :start_date (apple-time/appletime->emacs start_date))
	 (plist-put :rend_date (apple-time/appletime->emacs rend_date)))))
   events))

(defun applecal-events--make-date-from-date-time (date-for-time day month year)
  "Create a new date with the time from DATE-FOR-TIME, MONTH, YEAR, and DAY."
  (cl-destructuring-bind (second minute hour &rest _other-time-info)
      date-for-time
    (thread-first
      (make-decoded-time
       :second second :minute minute :hour hour :day day :month month :year year)
      encode-time
      decode-time)))

(defun applecal-events--filter-projected-events (decoded-start decoded-end events)
  "Filter out any projected EVENTS that are not in DECODED-START to DECODED-END window."
  (let ((result  '()))
    (dolist (event events)
      (cl-destructuring-bind
	    (&key rend_date xdate frequency interval specifier start_date end_date &allow-other-keys)
	  event
	(if (equal specifier :null)
	    (push event result)
	  (let* ((stop-date (or decoded-end
				rend_date
				(decoded-time-add
				 decoded-start (make-decoded-time :day 10))))

		 (xdates '()))
	    (dolist (exclude (json-parse-string xdate :array-type 'list))
	      (unless (equal exclude :null)
		(push (apple-time/appletime->emacs exclude) xdates)))
	    (dolist (date (apple-time/project-dates
			   start_date stop-date specifier frequency interval xdates))
	      (when (and
		     (or ;; this could probably be it's own function...
		      (apple-time/earlier-p decoded-start date)
		      (apple-time/date-equal-p decoded-start date))
		     (or (not decoded-end)
			 (apple-time/earlier-p date decoded-end)))
		(cl-destructuring-bind (_sec _min _hr day month year &rest _other)
		    date
		  (let* ((new-start (applecal-events--make-date-from-date-time
				     start_date day month year))
			 (new-end (applecal-events--make-date-from-date-time
				   end_date day month year))
			 (new-event  (thread-first
				       (cl-copy-list event)
				       (plist-put :start_date new-start)
				       (plist-put :end_date new-end))))
		    (push new-event result)))))))))
    result))

(defun applecal-events--equal? (event-one event-two)
  "Check if EVENT-ONE and EVENT-TWO have the same start, end, and title."
 (cl-destructuring-bind (&key start_date end_date title &allow-other-keys)
     event-one
   (and
    (equal start_date (plist-get event-two :start_date))
    (equal end_date (plist-get event-two :end_date))
    (equal title (plist-get event-two :title)))))

(defun applecal-events--dedupe-events (events)
  "Dedupe EVENTS based on APPLECAL--EVENT-EQUAL?."
  (let ((results '()))
    (dolist (event events)
      (unless (seq-some
	       (lambda (other-event)
		 (applecal-events--equal? event other-event))
	       results)
	(push event results)))
    results))

(defun applecal-events--decode-status (status-number)
  "Get a readable status from the STATUS-NUMBER for an event."
  (cond ((= status-number 0) :none)
	((= status-number 1) :confirmed)
	((= status-number 2) :tentative)
	((= status-number 3) :canceled)))

(defun applecal-events--apply-custom-transform (events)
  "Apply the custom transformation to any calendar EVENTS."
  (mapcar
   (lambda (event)
     (funcall *applecal-events/custom-transform* event))
   events))

(defun applecal-events--overlap?
    (start-time-one end-time-one start-time-two end-time-two)
  "Interval START-TIME-ONE to END-TIME-ONE overlaps START-TIME-TWO to END-TIME-TWO."
  ;; this is too complicated -- just check if start-one is between two
  ;; or start-two is between one
  (or
   (and
    (<= end-time-one end-time-two)
    (>= end-time-one start-time-two))
   (and
    (>= start-time-one start-time-two)
    (<= start-time-one end-time-two))
   (and
    (<= end-time-two end-time-one)
    (>= end-time-two start-time-one))
   (and
    (>= start-time-two start-time-one)
    (<= start-time-two end-time-one))))



(defun applecal-events--filter-by-date (start-time end-time entries)
  "Filter event ENTRIES based on START-TIME and END-TIME."
  (seq-filter
   (lambda (entry)
     (cl-destructuring-bind
	   (&key calendar rend_date start_date status end_date
		 specifier has_recurrences &allow-other-keys)
	 entry
       (and
	;; calendar is legit
	(or (not *applecal-events/calendars*)
	    (member calendar *applecal-events/calendars*))
	;; not canceled
	(not (equal :canceled (applecal-events--decode-status status)))
	;; not filtered out
	(funcall *applecal-events/custom-filter* entry)
	(or
	 ;; recurring event ending after end-time
	 (and (= has_recurrences 1)
	      (not (equal specifier :null))
	      (or (equal rend_date :null)
		  (>= rend_date end-time)))
	 ;; event overlaps with start and end times
	 (applecal-events--overlap? start-time end-time start_date end_date)
	;; (or
	;;  (>= start_date start-time)
	;;  (or (not end-time) (<= start_date end-time)))
	 ))))
   entries))

(cl-defun applecal-events/event-info (&key start-date end-date)
  "Get events between START-DATE and END-DATE as as plists from the calendar cache.

START-DATE and END-DATE are lists of the form (MONTH DAY YEAR).

If no START-DATE is specified then today is used.
If no END-DATE is specified then all events following the set or defaulted
START-DATE are gathered."
  (let* ((start-time (if start-date (apple-time/from-date start-date) (apple-time/today)))
	 (decoded-start (apple-time/appletime->emacs start-time))
	 (end-time (when end-date (apple-time/from-date end-date)))
	 (decoded-end (when end-date (apple-time/appletime->emacs end-time))))
    (thread-last
      (applecal-utils/event-info)
      (applecal-events--filter-by-date start-time end-time)
      applecal-events--convert-times
      (applecal-events--filter-projected-events decoded-start decoded-end)
      applecal-events--apply-custom-transform
      applecal-events--dedupe-events)))

(defun applecal-events/add
    (title description start-date start-time duration)
  "Add event with TITLE, DESCRIPTION, START-DATE, START-TIME, and DURATION to apple calendar."
  (let ((properties (apple/make-properties
		     "description" (apple/quote description)
		     "summary" (apple/quote title)
		     "start date" "startDate"
		     "end date" (format "startDate + %s * minutes" duration))))
    (apple/execute!
     (apple/progn
      (apple/tell-application
       "System Events"
       (apple/set "appRunning" (format "(name of processes) contains %s"
				       (apple/quote "Calendar"))))
      (apple/tell-application
       "Calendar"
       (apple/tell
	'(:item "Calendar" :type :calendar)
	(apple/set "startDate" (thread-last
				 start-time
				 (format  "%s at %s" start-date)
				 apple/quote
				 (format "date %s")))
	(format "make new event at end with properties %s" properties)))
      (apple/tell-application
       "Calendar"
       (apple/if "not appRunning" "quit")))
     :message (format "Creating %s for %s at %s" title start-date start-time)
     :timed? t)))

(defun applecal-events/delete (title start-date start-time)
  "Delete event with TITLE starting at START-DATE and START-TIME."
  (apple/execute!
   (apple/progn
    (apple/tell-application
     "Calendar"
     (apple/tell
      '(:type :calendar :item "Calendar")
      (apple/set "theSummary" (apple/quote title))
      (apple/set "startTime" (thread-last
			       start-time
			       (format "%s at %s" start-date)
			       apple/quote
			       (format "date %s")))
      (apple/set "theEventList" (string-join
				 (list
				  "every event where its summary is equal to "
				  "theSummary and start date is equal to startTime")))
      (apple/if "length of theEventList is less than 2"
		(apple/progn
		 (apple/set "theEvent" "the first item of theEventList")
		 "delete theEvent"))))
    (apple/tell-application "Calendar" "quit"))
   :message (format "Deleting event: %s" title)
   :timed? t))

(provide 'applecal-events)
;;; applecal-events.el ends here
