(in-package :matrix-framework)

;;; herin lies functions to 'visit' rooms.

;; (defun sync-general (&key (since nil) (filter nil) (full-state nil)
;; 		       (presence nil) (timeout nil))
;;   (recieve-json (concatenate 'string *homeserver* "_matrix/client/r0/sync?"
;; 			  (when since
;; 			    (concatenate 'string "&since=" since))
;; 			  (when filter
;; 			    (concatenate 'string "&filter=" filter))
;; 			  (when full-state
;; 			    (concatenate 'string "&full_state=" full-state))
;; 			  (when presence
;; 			    (concatenate 'string "&set_presence=" presence))
;; 			  (when timeout
;; 			    (concatenate 'string "&timeout=" timeout)))))

(defun get-more-room-events (room-id previous-batch &optional (limit 5) (dir "b"))
  (sync-general (cat "_matrix/client/r0/rooms/" room-id "/messages?")
		:additional (paginate :from previous-batch :limit limit :dir dir)))
