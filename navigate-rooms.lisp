(in-package :matrix-framework)

;;; herin lies functions to 'visit' rooms.

(defparameter *join-rooms* nil)
(defun init-*join-rooms* (&optional (chambers *chambers*))
  (setf *join-rooms* (rest (fourth chambers))))

(defparameter *room-id->name* '()
  "stores the name of the rooms, with the id as the key")
(defparameter *room-name->id* '())

(defun generate-names (&optional (chambers *chambers*))
  "this has been updated to use the matrix api for getting state things

this is... a mess. ok so were looping through the chambers, and for every 
room we are grabbing the state events,cause thats where the most current room
name is stored. then we loop through every event, checking to see if the 
content is of type name. if so we set the hash with the id as the key, and 
the name as the value. "
  (setf *room-id->name* nil)
  (loop for room in (rest (fourth chambers))
     do
       (setf *room-id->name*
	     (cons `(,(car room) . ,(cdar (sync-general (cat "_matrix/client/r0/rooms/"
						(car room) "/state/m.room.name/"))))
		   *room-id->name*)))
  (setf *room-name->id* (mapcar #'(lambda (pair)
				    `(,(cdr pair) . ,(car pair)))
				*room-id->name*)))

(defun generate-names-locally (&optional (chambers *chambers*))
  "this operates like generate-names, but will never call the api,
and only gets names from the state events. use this if your internet
connection is very slow, but know that not every room will be entered 
into the table of names"
  (loop for room in (rest (fourth chambers))
     do
       (let ((room-id (car room))
	     (state-events (cdadr (assoc "state" (cdr room) :test #'string=))))
	 (loop for event in state-events
	    do
	      (princ "test")
	      (terpri)))))

(defun search-for-room.name (events-list)
  (when (stringp (car events-list))
    (setf events-list (cdr events-list)))
  ())

(defun get-more-room-events (room-id previous-batch &key (limit 20) (dir "b"))
  "calls the message sync api, "
  (sync-general (cat "_matrix/client/r0/rooms/" room-id "/messages?")
		:additional (paginate :from previous-batch :limit limit :dir dir)))

(defun grab-single-room (room-name &optional (id nil)
				     ;; (chambers (cdr (fourth *chambers*)))
				     )
  "takes a name (which could be an id, if it is set id to t) and retrieves the
proper room. "
  ;; (mapcar chambers #'(lambda (room)
  ;; 		       ()))
  ;; (loop for room in chambers
  ;;    do
  ;;      ())
  (terpri)
  (let ((room-id (if id
		     id
		     (get-room-id-from-name room-name))))
    (princ room-id)
    (terpri)
    (terpri)
    (find-room room-id)))

(defun get-room-id-from-name (name &optional (db *room-name->id*))
  "looks up the room id from a name in the database 'db'"
  (cdr (assoc name db :test #'string=)))

(defun get-room-name-from-id (id &optional (db *room-id->name*))
  (cdr (assoc id db :test #'string=)))

(defun find-room (room-id &optional (room-list *join-rooms*))
  "takes a room id, and iterates over a list of rooms, returning
the one with a matching ID. "
  (loop for room in room-list
     do
       (when (string= (car room) room-id)
	 (return-from find-room room))))

()

