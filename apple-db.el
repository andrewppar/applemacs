;;; apple-db -- Summary
;;;
;;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;;; Version: 0.0.1
;;; Package-Requires: ((emacs "29.1") (f "0.20.0"))
;;; Keywords: email, macos
;;;
;;; Commentary:
;;;
;;; Mess around with Apple's sqlite dbs
;;;
;;; Code:
(require 'subr-x)
(require 'cl-lib)

(defconst *apple-db--sqlite*
  (thread-first "which sqlite3" shell-command-to-string string-trim))

;;;###autoload
(defvar *apple-db/ical-db*
  nil
  "Path to the ical sqlite db.")

;;;###autoload
(defvar *apple-db/mail-db*
  nil
  "Path to the mail db.")

(cl-defun apple-db/select (columns &key distinct?)
  "Create a select statements from COLUMNS
Optionally pass DISTINCT? to make it select distinct."
  (let ((query (list (string-join columns ", "))))
    (when distinct?
      (push "distinct" query))
    (push "select" query)
    (string-join query " ")))

(defun apple-db/column (&rest table-column-spec)
  (let ((table-or-column (car table-column-spec))
	column table spec column-expression)
    (if (stringp (cadr table-column-spec))
	(setq column (cadr table-column-spec)
	      table (car table-column-spec)
	      spec (cddr table-column-spec))
      (setq column (car table-column-spec)
	    spec (cdr table-column-spec)))
    (if table
	(setq column-expression (format "%s.%s" table column))
      (setq column-expression column))
    (cl-destructuring-bind (&key as &allow-other-keys)
	spec
      (if as
	  (format "%s as %s" column-expression as)
	column-expression))))

(defun apple-db/from (table)
  (format "from %s" table))

(cl-defun apple-db/join (table &key on join-type)
  (unless on
    (throw :exception ":on is required for joins"))
  (cl-case join-type
    (:left (format "left outer join %s on %s" table on))
    (:right (format "right outer join %s on %s" table on))
    (:outer (format "outer join %s on %s" table on))
    (:cross (format "cross join %s on %s" table on))
    (t (format "join %s on %s" table on))))

(defun apple-db/where (expression)
  (format "where %s" expression))

(defun apple-db/group-by (&rest columns)
 (format "group by %s" (string-join columns ", " )))

(defun apple-db/order-by (&rest columns)
  (format "order by %s"
	  (string-join
	   (mapcar
	    (lambda (column)
	      (if (listp column)
		  (if (equal (cadr column) :desc)
		      (format "%s desc" (car column))
		    (car column))
		column))
	    columns)
	   ", ")))

(cl-defun apple-db/limit (&key limit offset)
  (let ((limit-clause (when limit (format "limit %s" limit)))
	(offset-clause (when offset (format "offset %s" offset))))
    (string-join (remove nil (list limit-clause offset-clause)) "\n")))

(defun apple-db/query (&rest parts)
  (string-join parts "\n"))

;;; queries

(defconst *apple-db/events-query*
  (apple-db/query
   (apple-db/select
    (list (apple-db/column "Store" "name" :as "account")
	  (apple-db/column "Store" "type")
	  (apple-db/column "Calendar" "color")
	  (apple-db/column "Calendar" "title" :as "calendar")
	  (apple-db/column "Calendar" "subcal_url")
	  (apple-db/column "Calendar" "symbolic_color_name")
	  (apple-db/column "CAST(CalendarItem.end_date AS INT)" :as "end_date")
	  (apple-db/column "CAST(CalendarItem.orig_date AS INT)" :as "orig_date")
	  (apple-db/column "CAST(CalendarItem.start_date AS INT)" :as "start_date")
	  (apple-db/column "CAST(CalendarItem.end_date - CalendarItem.start_date AS INT)"
			   :as "duration")
	  (apple-db/column "CalendarItem" "all_day")
	  (apple-db/column "CalendarItem" "availability")
	  (apple-db/column "CalendarItem" "conference_url_detected")
	  (apple-db/column "CalendarItem" "has_recurrences")
	  (apple-db/column "CalendarItem" "invitation_status")
	  (apple-db/column "CalendarItem" "orig_item_id")
	  (apple-db/column "CalendarItem" "rowid")
	  (apple-db/column "CalendarItem" "start_tz")
	  (apple-db/column "CalendarItem" "status")
	  (apple-db/column "CalendarItem" "summary" :as "title")
	  (apple-db/column "CalendarItem" "unique_identifier")
	  (apple-db/column "CalendarItem" "url")
	  (apple-db/column "CalendarItem" "uuid")
	  (apple-db/column "json_group_array(DISTINCT CAST(ExceptionDate.date AS INT))"
			   :as "xdate")
	  (apple-db/column "json_group_array(DISTINCT Identity.display_name)"
			   :as "attendees")
	  (apple-db/column "Location" "address" :as "address")
	  (apple-db/column "Location" "title" :as "location")
	  (apple-db/column "Recurrence" "count")
	  (apple-db/column "CAST(Recurrence.end_date AS INT)" :as "rend_date")
	  (apple-db/column "Recurrence" "frequency")
	  (apple-db/column "Recurrence" "interval")
	  (apple-db/column "Recurrence" "specifier")
	  (apple-db/column "min(Alarm.trigger_interval)" :as "trigger_interval"))
    :distinct? t)
   (apple-db/from "Store")
   (apple-db/join "Calendar" :on "Calendar.store_id = Store.rowid")
   (apple-db/join "CalendarItem" :on "CalendarItem.calendar_id = Calendar.rowid")
   (apple-db/join "Location"
		  :on "CalendarItem.location_id = Location.rowid"
		  :join-type :left)
   (apple-db/join "Recurrence"
		  :on "Recurrence.owner_id = CalendarItem.rowid"
		  :join-type :left)
   (apple-db/join "ExceptionDate"
		  :on "ExceptionDate.owner_id = CalendarItem.rowid"
		  :join-type :left)
   (apple-db/join "Alarm"
		  :on "Alarm.calendaritem_owner_id = CalendarItem.rowid"
		  :join-type :left)
   (apple-db/join "Participant"
		  :on "Participant.owner_id = CalendarItem.rowid"
		  :join-type :left)
   (apple-db/join "Identity"
		  :on "Identity.rowid = Participant.identity_id"
		  :join-type :left)
   (apple-db/where "Store.disabled IS NOT 1")
   (apple-db/group-by "CalendarItem.rowid")
   (apple-db/order-by "CalendarItem.unique_identifier")))

(defun apple-db/execute! (database query)
  "Run QUERY against DATABASE."
  (thread-first
    (format "%s -json %s \"%s\"" *apple-db--sqlite* (expand-file-name database) query)
    shell-command-to-string
    (json-parse-string :object-type 'plist :array-type 'list)))

(provide 'apple-db)
;;; apple-db.el ends here
