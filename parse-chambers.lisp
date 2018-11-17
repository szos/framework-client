(in-package :matrix-framework)

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
  (mapcar #'(lambda (tl)
		(let ((room-id (car tl))
		      (line (cdadr tl)))
		  (list room-id (parse-timeline-events
				 (get-events-from-timeline line)))))
	    room+timeline-list))

;; example function line:
(get-events-from-timeline (parse-all-timelines ))
