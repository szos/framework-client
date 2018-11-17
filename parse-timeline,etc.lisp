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
    (when (string= type "m.room.redaction")
      (print type))
    (cond ((string= type "m.room.message")
	   (format-message-event event))
	  ((string= type "m.room.member")
	   (format-member-event event))
	  ((string= type "m.room.encrypted")
	   (format-encrypted-event event))
	  ((string= type "m.room.redaction")
	   (format-redacted-event event))
	  ((string= type "m.room.history_visibility")
	   (format-room-history-visible& event))
	  ((string= type "m.room.guest_access")
	   (format-room-guest-access& event))
	  ((string= type "m.room.name")
	   (format-room-name& event))
	  ((string= type "m.room.power_levels")
	   (format-room-power-levels& event))
	  ((string= type "m.room.join_rules")
	   (format-room-join-rules& event))
	  ((string= type "m.room.avatar")
	   (format-room-avatar event))
     	  
	  ((string= (subseq type 0 7) "m.call.")
	   (format-call-events event))
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

(defun format-encrypted-event (event)
  "takes an event of type m.room.encrypted, and returns it formatted. if the client 
has the keys, it will decrypt the message. otherwise it says its cipher text / encrypted."
  (let ((sender (cdr (assoc "sender" event :test #'string=))))
    (format nil "~A says:~%    encryption is not yet implemented." sender)))

(defun format-redacted-event (event)
  "takes a redaction message. it should replace the text of the referenced event with
the word redacted, but events arent yet indexed to permit this. "
  (let ((sender (cdr (assoc "sender" event :test #'string=)))
	(event-to-redact (cdr (assoc "redacts" event :test #'string=))))
    (format nil "~A redacts event ~A" sender event-to-redact)))

(defun format-call-events (event)
  "this client cant deal with calls. "
  (declare (ignore event))
  (format nil "this client cannot accept or place calls"))

(defun format-room-name& (event)
  "this function and others like it should have side effects of setting our 
rooms metadata. in this case it should setf the room name to whatever we recieve. 
however room names are not yet implemented. for types m.room.name"
  (let ((sender (cdr (assoc "sender" event :test #'string=)))
	(room-name (cdr (assoc "name" (cdr (assoc "content" event :test #'string=)) :test #'string=))))
    (format nil "~A set the room name to ~A" sender room-name)))

(defun format-room-guest-access& (event)
  "takes events of type m.room.guest_access and returns a string indicating whether
guests can access this room. it should also alter our data regarding the room, 
like format-room-name&"
  (let ((sender (cdr (assoc "sender" event :test #'string=)))
	(access? (cdr (assoc "guest_access" (cdr (assoc "content" event :test #'string=)) :test #'string=))))
    (if (string= access? "can_join")
	(format nil "Guests can join. Set by ~A" sender)
	(format nil "Guests cannot join. Set by ~A" sender))))

(defun format-room-history-visible& (event)
  (let ((sender (cdr (assoc "sender" event :test #'string=)))
	(visibility-level (cdr (assoc "history_visibility"
				      (cdr (assoc "content" event :test #'string=))
				      :test #'string=))))
    (format nil "The history of the room was set to ~A by ~A" visibility-level sender)))

(defun format-room-power-levels& (event)
  (let ((sender (cdr (assoc "sender" event :test #'string=))))
    (format nil "~A has set the power levels. this function is not fully implemented" sender)))

(defun format-room-join-rules& (event)
  (let ((sender (cdr (assoc "sender" event :test #'string=)))
	(join-rule (cdr (assoc "join_rule"
			       (cdr (assoc "content" event :test #'string=))
			       :test #'string=))))
    (format nil "~A set the rules regarding joining to ~A" sender join-rule)))

(defun format-room-avatar (event)
  (let ((sender (cdr (assoc "sender" event :test #'string=))))
    (format nil "~A changed their avatar" sender)))
