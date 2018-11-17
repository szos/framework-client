(in-package :matrix-framework)

;;; all the classes and methods should go here. preferablyy rework everything to be
;;; useing methods, so that for example we just have to have our main loop running and everything
;;; else is method dispatches for updateing rooms and objects.

(defclass chamber ()
  ;; called a chamber as room is not a valid name (locks on common-lisp package)
  ;; this should hold on to the information needed for a specific room. 
  ((previous-batch :initarg :previous-batch
		   :reader previous-batch
		   :writer (setf previous-batch)
		   :documentation "holds the previous batch token for the 
earliest chunk of data in the room. ")
   (room-name :initarg :name
	      :reader room-name
	      :writer (setf room-name))
   (state :initarg :state
	  :reader state
	  :writer (setf state)
	  :documentation "this holds room state.
this should be stored like (event-type (state-key . full event))
for example:
\(\(\"m.room.member\" \(\"\@mabir:matrix.org\" . event\)\)
 \(\"m.room.name\" \(\"\" . event\)\)\) "
;;; in other words '(("m.room.member" ("@mabir:matrix.org" . event))
;;;                  ("m.room.name" ("" . event)))	  
	  )
   
   (current-state :initarg :current-state
		  :reader current-state
		  :writer (setf current-state))
   (notifications :initarg :notifications
		  :reader notifications
		  :writer (setf notifications)
		  :documentation "notifications! yay!")
   (summary :initarg :summary
	    :reader summary
	    :writer (setf summary)
	    :documentation "summary of room")
   (ephemeral :initarg :ephemeral
	      :reader ephemeral
	      :writer (setf ephemeral)
	      :documentation "transient, non stored things. ")
   (timeline :initarg :timeline
	     ;; this is where we should be formatting our output to the user
	     ;; from. This holds all events that matter, and then we access
	     ;; previous events via the /messages api
	     :reader timeline
	     :writer (setf timeline)
	     :documentation "this should contain a timeline-container
object. setting the timeline slot should be accompanied by a call to
the formatting method for timeline-container. ")
   (account-data :initarg :account
		 :reader account
		 :writer (setf account)
		 :documentation "houses account_data field
this holds specific information on a room. ")))


(defmethod update-room-from-sync ((old-room chamber) (new-room chamber))
  ;;; let some variables:
  ;;; then do this.
  (make-instance 'chamber
		 :timeline (update-timeline old-room )))

;;; dont track ephemeral events. we can hack in support for those later.
;;; dont worry about the current-state. In fact, dont worry about anything except
;;; room-name, timeline, and account-data. with these three things we should be
;;; able to update everything as needed, and we can update from there.
;;; we sort based on event type. eg m.room* is an event specific to the room.
;;; so if we just track all these events and we can update based only on the timeline.
;;; this wont give us anything like state data, but we can parse the data looking for
;;; any and all state keys, and this makes everything easier. 

(defmethod update-timeline ((old-room chamber) timeline &optional (where :next))
  (let ((new-timeline (if (eq where :previous)
			  (cat-timeline (timeline old-room) timeline)
			  (cat-timeline timeline (timeline old-room)))))
    ())
  (cond ((eq where :next)
	 ;;
	 )
	((eq where :previous)
	 ;;
	 )
	(t
	 nil)))
