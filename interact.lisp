(in-package :matrix-framework)

;;; this file is for holding our custom repl.

(defparameter *allowed-commands*
  '(logout smart-login initialize quit exit grab-single-room view-room))

(defparameter *repl-persist?* t)

(defun test-repl ()
  (setf *repl-persist?* t)
  (smart-login)
  (loop while *repl-persist?* do
       (test-print (test-eval (test-read)))))

(defun test-read ()
  (princ "command:  ")
  (let ((cmd (read-from-string ;; lets us input commands without parens.
	      (cat "(" (read-line) ")"))))
    (cond ((or (equal cmd '(quit)) (equal cmd '(exit)))
	   (logout)
	   (princ "logging out and exiting application")
	   (terpri)
	   '(quit))
	  (t
	   cmd))))

(defun quit ()
  (setf *repl-persist?* nil))

(defun test-eval (form)
  (if (member (car form) *allowed-commands*)
      (eval form)
      "Form Not Permitted"))

(defun test-print (value)
  (when value (princ value))
  (terpri))

(defun view-room (name)
  (display-room-events-list (parse-timeline-events (get-events-from-timeline (get-timeline-from-room (grab-single-room name))))))
