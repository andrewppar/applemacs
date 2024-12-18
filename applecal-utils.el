;;; applecal-time --- Summary
;;;
;;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;;; Version: 0.0.1
;;; Package-Requires: ((emacs "29.1") (f "0.20.0"))
;;; Keywords: email, macos
;;;
;;; Commentary:
;;;
;;; Access Apple's secret calendar sqlite db
;;;
;;; Code:
(require 'subr-x)

(defconst *applecal--sqlite*
  (thread-first "which sqlite3" shell-command-to-string string-trim))

;;;###autoload
(defvar *applecal/ical-db*
  nil
  "Path to the ical sqlite db.")

(defconst *applecal--db-events-query*
  (string-join
   (list
    "SELECT DISTINCT"
    "Store.name AS account,"
    "Store.type,"
    "Calendar.color,"
    "Calendar.title AS calendar,"
    "Calendar.subcal_url,"
    "Calendar.symbolic_color_name,"
    "CAST(CalendarItem.end_date AS INT) AS end_date,"
    "CAST(CalendarItem.orig_date AS INT) AS orig_date,"
    "CAST(CalendarItem.start_date AS INT) AS start_date,"
    "CAST(CalendarItem.end_date - CalendarItem.start_date AS INT) AS duration,"
    "CalendarItem.all_day,"
    "CalendarItem.availability,"
    "CalendarItem.conference_url_detected,"
    "CalendarItem.has_recurrences,"
    "CalendarItem.invitation_status,"
    "CalendarItem.orig_item_id,"
    "CalendarItem.rowid,"
    "CalendarItem.start_tz,"
    "CalendarItem.status,"
    "CalendarItem.summary AS title,"
    "CalendarItem.unique_identifier,"
    "CalendarItem.url,"
    "CalendarItem.uuid,"
    "json_group_array(DISTINCT CAST(ExceptionDate.date AS INT)) AS xdate,"
    "json_group_array(DISTINCT Identity.display_name) AS attendees,"
    "Location.address AS address,"
    "Location.title AS location,"
    "Recurrence.count,"
    "CAST(Recurrence.end_date AS INT) AS rend_date,"
    "Recurrence.frequency,"
    "Recurrence.interval,"
    "Recurrence.specifier,"
    "min(Alarm.trigger_interval) AS trigger_interval"
    "FROM Store"
    "JOIN Calendar ON Calendar.store_id = Store.rowid"
    "JOIN CalendarItem ON CalendarItem.calendar_id = Calendar.rowid"
    "LEFT OUTER JOIN Location ON Location.rowid = CalendarItem.location_id"
    "LEFT OUTER JOIN Recurrence ON Recurrence.owner_id = CalendarItem.rowid"
    "LEFT OUTER JOIN ExceptionDate ON ExceptionDate.owner_id = CalendarItem.rowid"
    "LEFT OUTER JOIN Alarm ON Alarm.calendaritem_owner_id = CalendarItem.rowid"
    "LEFT OUTER JOIN Participant ON Participant.owner_id = CalendarItem.rowid"
    "LEFT OUTER JOIN Identity ON Identity.rowid = Participant.identity_id"
    "WHERE Store.disabled IS NOT 1"
    "GROUP BY CalendarItem.rowid"
    "ORDER BY CalendarItem.unique_identifier")
   "\n"))

(defun applecal--run-db-query (query)
  "Run QUERY against *applecal/ical-db*."
  (thread-first
    "%s -json %s '%s'"
    (format *applecal--sqlite* (expand-file-name *applecal/ical-db*) query)
    (shell-command-to-string)
    (json-parse-string :object-type 'plist :array-type 'list)))

(defun applecal-utils/event-info ()
  "Get the event infor from *applecal/ical-db*."
  (applecal--run-db-query *applecal--db-events-query*))

(provide 'applecal-utils)
;;; applecal-utils.el ends here
