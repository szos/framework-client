(in-package :matrix-framework)

;;; what i want is a timeline, to be then printed like so:
;;; timestamp user: message

;;; what we have now is:
;;; user says:
;;;     message
;;; ------------- (for 80 characters. we assume minimum of 80 character display)

(defun display-room-events-list (list-of-formatted-events)
  (loop for event in list-of-formatted-events
     do
       (princ event)
       (terpri)
       (princ "--------------------------------------------------------------------------------")
       (terpri)))

(defun get-events-from-timeline (timeline)
  "takes a timeline and returns the 'events' portion of the timeline"
  (let ((events (assoc "events" (rest timeline) :test #'string=)))
    events))

(defun parse-timeline-events (list-of-events)
  "takes a list of events, and returns a list of strings, one for each event, 
that are ready to be displayed. 

currently only 
m.room.message, m.room.member, 
 types of messages are working"
  (when (stringp (car list-of-events))
    (setf list-of-events (rest list-of-events)))
  (mapcar #'parse-single-event list-of-events))

(defun parse-single-event (event)
  "takes in one event, and returns that event formatted and ready for displaying. 
it also updates the room, based on the 'unsigned' portion of the event. this means
side effects, as its calling a function that will change some data. "
  (let ((unsigned (cdr (assoc "unsigned" event :test #'string=)))
	(type (cdr (assoc "type" event :test #'string=))))
    ;; (update-room unsigned) ;; this should check for 'replaces_state' among others and update the room to reflect any changes.
    (cond ((string= type "m.room.message")
	   (format-message-event event))
	  ((string= type "m.room.member")
	   (format-member-event event))
	  (t
	   "this message type has not been implemented"))))

(defun format-member-event (event)
  "takes in an event of type m.room.member and formats it, returning the formatted text"
  (let* ((sender (cdr (assoc "sender" event :test #'string=)))
	 (content (cdr (assoc "content" event :test #'string=)))
	 (member? (cdr (assoc "membership" content :test #'string=))))
    (if (string= member? "leave") ;; theres only leave/join possible
	(format nil "~A has left the room" sender)
	(format nil "~A (~A) has joined the room"
		(cdr (assoc "displayname" content :test #'string=)) sender))))

(defun format-message-event (event)
  "takes in one event that is of type m.room.message and formats it, returning the formatted
text. "
  (let ((sender (cdr (assoc "sender" event :test #'string=)))
	(text (let ((content (cdr (assoc "content" event :test #'string=))))
		(when (string= "m.text" (cdr (assoc "msgtype" content :test #'string=)))
		  (cdr (assoc "body" content :test #'string=))))))
    (format nil "~a says:~%    ~a" sender text)))
