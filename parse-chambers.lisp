(in-package :matrix-framework)

(defun display-room-events-list (list-of-formatted-events)
  (loop for event in list-of-formatted-events
     do
       (princ event)
       (terpri)
       (princ "--------------------------------------------------------------------------------")
       (terpri)))

(defun display-all-rooms-events-list (list-of-rooms-w/-formatted-events)
  (loop for room in list-of-rooms-w/-formatted-events
     do
       (princ "Room ID:  ")
       (print (car room))
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
  "this takes in a list of lists, structured as so:
((\"room-id\" (timeline...))
 (\"room-id\" (timeline...))) 
and returns every timeline parsed via parse-timeline-events."
  (mapcar #'(lambda (tl)
		(let ((room-id (car tl))
		      (line (cdadr tl)))
		  (list room-id (parse-timeline-events
				 (get-events-from-timeline line)))))
	    room+timeline-list))

;; example function line:
;; (get-events-from-timeline (parse-all-timelines ))

