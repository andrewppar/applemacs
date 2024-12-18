;;; applecal-utils --- Summary
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
(require 'apple-db)

(defun applecal-utils/event-info ()
  "Get the event infor from *applecal/ical-db*."
  (apple-db/execute! *apple-db/ical-db* *apple-db/events-query*))

(provide 'applecal-utils)
;;; applecal-utils.el ends here
