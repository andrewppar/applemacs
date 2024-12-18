;;; applecal --- Summary
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
(require 'applecal-events)

;;;###autoload
(defun applecal/add-event (title description start-date start-time duration)
  "Add event with to calendar.
Prompts for TITLE, DESCRIPTION, START-DATE, START-TIME, and DURATION."
  (interactive
   (list
    (read-string "title: ")
    (read-string "description: ")
    (read-string "date (month-day-year format): "
		 (format-time-string "%m-%d-%Y"))
    (read-string "start time: "
		 (format-time-string "%H:%M"))
    (read-string "duration (in minutes): " "30")))
  (applecal-events/add title description start-date start-time duration))

;;;###autoload
(defun applecal/delete-event
    (title start-date start-time)
  "Delete event with TITLE at START-DATE and START-TIME."
  (interactive
   (list
    (read-string "title: ")
    (read-string "start date: ")
    (read-string "start time: ")))
  (applecal-events/delete title start-date start-time))

(defun applecal--events-from-now-until (plus-days)
  "Get the calendar events between now and PLUS-DAYS."
  (let ((tomorrow (thread-first
		    (decode-time)
		    (decoded-time-add (make-decoded-time :day plus-days))
		    encode-time
		    decode-time)))
    (cl-destructuring-bind (_sec _min _hr day month year &rest other_info)
	tomorrow
      (applecal-events/event-info :end-date (list year month day)))))

;;;###autoload
(defun applecal/todays-events ()
  "Get the calendar events for today."
  (applecal--events-from-now-until 1))

;;;###autoload
(defun applecal/next-weeks-events ()
  "Get the events for the upcoming week from calendar."
  (applecal--events-from-now-until 7))

(provide 'applecal)
;;; applecal.el ends here
