;;; applemail --- Summary
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
(require 'applemail-ui)

;;;###autoload
(defun applemail/inbox ()
  "Show the messages in your inbox."
  (interactive)
  ;; make this into a reset function
  (setq *applemail-ui/inbox-messages* (applemail-ui/inbox-messages))
  (applemail-display/refresh-inbox t))

(provide 'applemail)
;;; applemail.el ends here
