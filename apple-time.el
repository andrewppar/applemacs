;;; apple-time --- Summary
;;;
;;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;;; Version: 0.0.1
;;; Package-Requires: ((emacs "29.1") (f "0.20.0"))
;;; Keywords: email, macos
;;;
;;; Commentary:
;;;
;;; Perform time operations with applecal event information
;;;
;;; Code:
(require 'subr-x)
(require 'time-date)

(defun apple-time/appletime->emacs (apple-time)
  "Convert an integer representing APPLE-TIME to Emacs time."
  (if (numberp apple-time)
      (let* ((seconds-per-day (* 60 60 24))
	     (padding (+
		       ;; non-leap years
		       (* seconds-per-day 365 23)
		       ;; leap years
		       (* seconds-per-day 366 8))))
	(decode-time (+ apple-time padding)))
    apple-time))

(defun apple-time/emacs->appletime (emacs-time)
  "Convert EMACS-TIME to appletime."
  (let* ((seconds-per-day (* 60 60 24))
	 (padding (+
		   ;; non-leap years
		   (* seconds-per-day 365 23)
		   ;; leap years
		   (* seconds-per-day 366 8))))
    (thread-first
      (format-time-string "%s" (encode-time emacs-time))
      string-to-number
      (- padding))))

(defun apple-time/today ()
  "Get midnight today in applecal seconds (seconds from 1993 :/)."
  (let ((time (decode-time)))
    (setf (decoded-time-second time) 0)
    (setf (decoded-time-minute time) 0)
    (setf (decoded-time-hour time) 0)
    (apple-time/emacs->appletime time)))

(defun apple-time/from-date (date)
  "Transform DATE into an appletime.

DATE is expected to be a list whose first elements are YEAR, MONTH, DAY."
  (cl-destructuring-bind (year month day &rest _other)
      date
    (thread-first "%04d-%02d-%02d"
		  (format year month day)
		  parse-time-string
                  (decoded-time-set-defaults (cadr (current-time-zone)))
		  apple-time/emacs->appletime)))

(defun apple-time--nth-day-of-month (month year n)
  "Get the Nth day for MONTH in YEAR."
  (thread-first
    (make-decoded-time :day n :month month :year year)
    decoded-time-set-defaults
    encode-time
    decode-time))

(defun apple-time--last-day-of-month (month year)
  "Get the last day of the MONTH in YEAR."
  (apple-time--nth-day-of-month
   month year (date-days-in-month year month)))

(defun apple-time--nth-weekday-of-month (month year weekday n)
  "Get the Nth WEEKDAY of the MONTH in YEAR.
E.g. 3rd thursday of october 2024 => (0 0 0 17 10 2024...)"
  (let* ((max-iteration (abs n))
	 (limit-day (if (> n 0) 1 (date-days-in-month year month)))
	 (first-date-at-weekday-fn (lambda (target-weekday limit-weekday)
				     (if (> n 0)
					 (+ limit-day
					    (if (<= target-weekday limit-weekday)
						(- (+ 7 target-weekday) limit-weekday)
					      (- target-weekday limit-weekday)))
				       (- limit-day
					  (if (<= target-weekday limit-weekday)
					      (- limit-weekday target-weekday)
					    (+ limit-weekday (- 7 target-weekday)))))))
	 (limit-date (apple-time--nth-day-of-month month year limit-day))
	 (limit-weekday (decoded-time-weekday limit-date))
	 (acc (if (equal weekday limit-weekday)
		  limit-date
		  (apple-time--nth-day-of-month month year
					    (funcall first-date-at-weekday-fn
						     weekday limit-weekday))))
	 (iteration 1)
	 (day-exists? t))
    (while (and (< iteration max-iteration) day-exists?)
      (let ((next (thread-first
		    acc
		    (decoded-time-add (make-decoded-time :day (if (> n 0) 7 -7)))
		    encode-time
		    decode-time)))
	(if (= (decoded-time-month next) month)
	    (setq acc next
		  iteration (+ 1 iteration))
	  (setq acc nil
		day-exists? nil))))
    acc))

;; Recurring Events

(defun apple-time--parse-weekday-specifier (weekday-specifier)
  "Convert WEEKDAY-SPECIFIER into a repeat spec and a daynum."
  (let* ((repeat-spec (string-to-number (substring weekday-specifier 0 -2)))
	 (weekday (substring weekday-specifier -2))
	 (daynum  (alist-get
		   weekday
		   (seq-map-indexed
		    (lambda (item idx) (cons item idx))
		    '("SU" "MO" "TU" "WE" "TH" "FR" "SA"))
		   nil nil #'equal)))
    (list repeat-spec daynum)))

(defun apple-time--specifiers->weekdays (specifier)
  "Parse SPECIFIER into repetition and weekday specifiers."
  (mapcar #'apple-time--parse-weekday-specifier (split-string specifier ",")))

(defun apple-time-->seconds (date)
  "Convert date to seconds after epoch."
  (thread-last date encode-time (format-time-string "%s") string-to-number))

(defun apple-time/earlier-p (date-one date-two)
  "Check if DATE-ONE is earlier than DATE-TWO."
  (< (apple-time-->seconds date-one) (apple-time-->seconds date-two)))

(defun apple-time/later-p (date-one date-two)
  "Check if DATE-ONE is later than DATE-TWO."
  (> (apple-time-->seconds date-one) (apple-time-->seconds date-two)))

(defun apple-time--project-day-by-spec-one-step (date spec)
  "Project one step DATE by SPEC."
  (cl-destructuring-bind (repeat target-weekday)
      spec
    (if (= repeat 0)
	(let* ((weekday (decoded-time-weekday date))
	       (offset (if (<= target-weekday weekday)
			   (- (+ 7 target-weekday) weekday)
			 (- target-weekday weekday))))
	  (thread-first
	    date
	    (decoded-time-add (make-decoded-time :day offset))
	    encode-time
	    decode-time))
      (let* ((current-month (decoded-time-month date))
	     (current-year (decoded-time-year date))
	     (current-nth (apple-time--nth-weekday-of-month
			   current-month current-year target-weekday repeat)))
	(if (and current-nth (apple-time/earlier-p date current-nth))
	    current-nth
	  (let ((result nil))
	    (while (not result)
	      (let* ((month (if (= current-month 12) 1 (+ 1 current-month)))
		     (year (if (= month 1) (+ 1 current-year) current-year)))
		(setq result
		      (apple-time--nth-weekday-of-month month year target-weekday repeat)
		      current-month month
		      current-year year)))
	    result))))))

(defun apple-time--project-date-by-specifier (start-date end-date spec)
  "Project START-DATE until END-DATE with SPEC."
  (let ((result (list start-date)))
    (while (apple-time/earlier-p (car result) end-date)
      (push (apple-time--project-day-by-spec-one-step (car result) spec) result))
    result))

(defun apple-time--filter-by-interval (events interval)
  "Keep events from EVENTS at INTERVAL."
  (let ((interval-count interval)
	(result '()))
    (dolist (event (reverse events))
      (if (= interval-count interval)
	  (progn
	    (push event result)
	    (setq interval-count 1))
	(setq interval-count (+ interval-count 1))))
    (reverse result)))

(defun apple-time/equal-p (decoded-time-one decoded-time-two)
  "Check if the date of DECODED-TIME-ONE is the same as DECODED-TIME-TWO."
  (cl-destructuring-bind (_sec _min _hour day-one month-one year-one &rest _other)
      decoded-time-one
    (cl-destructuring-bind (_sec _min _hour day-two month-two year-two &rest _other)
	decoded-time-two
      (and (equal day-one day-two)
	   (equal month-one month-two)
	   (equal year-one year-two)))))


(defun apple-time--excluded-date? (date exclusion-dates)
  "Check whether DATE is a member of EXCLUSION-DATES.
Matches on DAY, MONTH, and YEAR, but nothing else."
  (let ((excluded? nil))
    (dolist (exclusion-date exclusion-dates)
      (when (apple-time/equal-p date exclusion-date)
	(setq excluded? t)))
    excluded?))

(defun apple-time--project-days (start-date end-date specifiers xdates)
  "Get all the projected times for SPECIFIERS between START-DATE and END-DATE."
  (seq-filter
   (lambda (date)
     (not (apple-time--excluded-date? date xdates)))
   (mapcan
    (lambda (spec)
      (apple-time--project-date-by-specifier start-date end-date spec))
    (apple-time--specifiers->weekdays specifiers))))

(defun apple-time--project-month-by-daynum (start-date end-date daynum)
  "Get every DAYNUMth of a month between START-DATE and END-DATE."
  (let* ((start-month (decoded-time-month start-date))
	 (start-year (decoded-time-year start-date))
	 (start-monthday (apple-time--nth-day-of-month
			  start-month start-year daynum))
	 (seed-date (if (apple-time/earlier-p
			 start-monthday start-date)
			(apple-time--nth-day-of-month
			 (if (= start-month 12) 1 (+ 1 start-month))
			 (if (= start-month 12) (+ 1 start-year) start-year)
			 daynum)))
	 (result (list seed-date)))
    (while (apple-time/earlier-p (car result) end-date)
      (let ((current-month (decoded-time-month (car result)))
	    (current-year (decoded-time-year (car result))))
	(push
	 (apple-time--nth-day-of-month
	  (if (= current-month 12) 1 (+ 1 current-month))
	  (if (= current-month 12) (+ 1 current-year) current-year)
	  daynum)
	 result)))
    result))

(defun apple-time--project-months (start-date end-date specifiers xdates)
  "Get projected time for monthly SPECIFIERS between START-DATE and END-DATE."
  (seq-filter
   (lambda (date)
     (not (apple-time--excluded-date? date xdates)))
   (mapcan
    (lambda (specifier)
      (let ((daynum (string-to-number specifier)))
	(apple-time--project-month-by-daynum start-date end-date daynum)))
    (string-split specifiers ","))))

(defun apple-time--project-year (start-date end-date)
  "Projected dates for annual events between START-DATE and END-DATE."
  ;; this isn't right, specifiers are broken into parts by ;
  ;; each part can have a time component of D,M,O,or S for day of week,
  ;; day of month, month of year, and nth(???)
  ;; The other component indicates how to interpret the first component..
  ;; see https://github.com/ajrosen/icalPal/blob/564851a4e6629919ef189a1d0c55cfcc876380d2/lib/event.rb#L198
  (let ((result (list start-date)))
    (while (apple-time/earlier-p (car result) end-date)
      (thread-first
	result
	car
	(decoded-time-add (make-decoded-time :year 1))
	encode-time
	decode-time
	(push result)))
    result))

(defun apple-time/project-dates
    (start-date end-date full-specifier frequency interval xdates)
  "Project FULL-SPECIFIER from START-DATE until END-DATE."
  (if (and (equal frequency 4) (equal full-specifier ""))
      (apple-time--project-year start-date end-date)
    (cl-destructuring-bind (scale specifiers)
	(split-string full-specifier "=")
      (cond ((equal scale "D")
	     (apple-time--filter-by-interval
	      (apple-time--project-days start-date end-date specifiers xdates)
	      interval))
	    ((equal scale "M")
	     (apple-time--filter-by-interval
	      (apple-time--project-months start-date end-date specifiers xdates)
	      interval))
	    (t
	     (error
	      (format "Cannot understand %s as a recurrence specifier"
		      full-specifier)))))))

(provide 'apple-time)
;;; apple-time.el ends here
