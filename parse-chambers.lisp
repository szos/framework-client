(in-package :matrix-framework)

;; (defun display-room-events-list (list-of-formatted-events)
;;   (loop for event in list-of-formatted-events
;;      do
;;        (princ event)
;;        (terpri)
;;        (princ "--------------------------------------------------------------------------------")
;;        (terpri)))

(defun display-all-rooms-events-list (list-of-rooms-w/-formatted-events)
  (loop for room in list-of-rooms-w/-formatted-events
     do
       (princ "Room ID:  ")
       (princ (car room))
       (terpri)
       (loop for event in (cadr room)
	  do
	    (princ event)
	    (terpri)
	    (princ "--------------------------------------------------------------------------------")
	    (terpri))
       (terpri)
       (terpri)
       (terpri)))

(defun get-room-timelines-from-*chambers* (&optional (rooms *chambers*))
  "takes in chambers, and converts it into a list of rooms and their timelines. "
  (setf rooms (cdr (cadddr rooms)))
  ;; rooms is now a list of rooms
  (let ((timeline-list (mapcar #'(lambda (room)
				   `(,(pop room) ,(assoc "timeline" room :test #'string=)))
				rooms))) ;; perhaps we should replace the room id with the room name?
    ;; (mapcar #'(lambda (tl)
    ;; 	      (list (car tl) (cadr tl)))
    ;; 	    timeline-list)
    timeline-list
    ))

(defun parse-all-timelines (room+timeline-list)
  "this takes in a list of lists, structured as so:"
;; ((\"room-id\" (timeline...))
;;  (\"room-id\" (timeline...))) 
;; and returns every timeline parsed via parse-timeline-events.
  (mapcar #'(lambda (tl)
		(let ((room-id (car tl))
		      (line (cdadr tl)))
		  (list room-id (parse-timeline-events
				 (get-events-from-timeline line)))))
	  room+timeline-list))

(defun translate-chambers (&optional (chambers *join-rooms*))
  (mapcar #'translate-room chambers))

(defun translate-room (room)
  "takes a room WITH an ID
then it.... does something? what? idk.  
well, its called from translate-chambers, and takes a single room. it
translates this room into its formatted counterpart, and stores it
in a variable. "
  ;; (when (stringp (car room)) ;; strip the name/id from the room. 
  ;;   (setf room (cdr room)))
  (let ((room-name (get-room-name-from-id (car room)))
	(state-variables (mapcar #'gather-state-variables
				 (cdr (assoc
				       "events"
				       (cdr (assoc
					     "state"
					     (rest room)
					     :test #'string=))
				       :test #'string=))))
	(timeline-events (mapcar #'gather-timeline-events
				 (cdr (assoc "events" (cdr
						       (assoc "timeline"
							      (rest room)
							      :test #'string=))
					     :test #'string=))))) ;; grab the events from the timeline
    `(,room-name
      (state
       ,state-variables)
      (timeline
       ,timeline-events))))

(defun gather-state-variables (state-events)
  (let ((members nil)
	(join-rules nil))
    ))

(defun gather-timeline-events (timeline-event)
  (let ((type (cdr (assoc "type" event :test #'string=))))
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
