;;;; matrix-framework.lisp

(in-package #:matrix-framework)

;;; "matrix-framework" goes here. Hacks and glory await!
(defparameter *session-user-auth* "MDAxOGxvY2F0aW9uIG1hdHJpeC5vcmcKMDAxM2lkZW50aWZpZXIga2V5CjAwMTBjaWQgZ2VuID0gMQowMDIzY2lkIHVzZXJfaWQgPSBAc3pvczptYXRyaXgub3JnCjAwMTZjaWQgdHlwZSA9IGFjY2VzcwowMDIxY2lkIG5vbmNlID0gMjhmR3ZHS2NFeEh4MFNFVAowMDJmc2lnbmF0dXJlID_IFoP5V3QWN3bq14DrV-W-PjCnXxG8tme8dGOFQEztCg")
(defparameter *chambers* nil
  "holds an alist of room ids and chamber class instances.")
(defparameter *settings* nil)
(defparameter *sync-batch-token* nil
  "holds the batch token, which tells us how much to sync. need to implement
a pagination function. ")
(defparameter *homeserver* "https://matrix.org/"
  "holds the homeserver string. set upon login, then should be locked. ")

(defun cat (&rest strs)
  (concatenate 'string strs))

(defun paginate (path &key (to nil) (from nil) (limit nil) (dir nil))
  "returns a string for paginating based on what was sent in. "
  (when (or to from limit dir)
    (cat (when from
	   (format nil "&from=~A" from))
	 (when to
	   (format nil "&to=~A" to))
	 (when limit
	   (format nil "&limit=~A" limit))
	 (when dir
	   (format nil "&dir=~A" dir)))))

(defun aquire-public-rooms (&optional (paginate nil))
  (recieve-json (cat *homeserver* "_matrix/client/r0/publicRooms"
		     )))

(defun post-room-to-directory ())

(defun initialize (&optional (data nil data-provided-p))
  (multiple-value-bind (next-batch chambers) ;; account)
      (parse-sync (if data-provided-p
		      data
		      (sync-general)))
    (setf *chambers* chambers)
    ;; (setf *settings* account)
    (setf *sync-batch-token* (cdr next-batch))))

(defun parse-sync (sync-data)
  (let ((room-list (assoc "rooms" sync-data :test #'string=))
	(next-batch (assoc "next_batch" sync-data :test #'string=))
	(room-holder nil))
    (setf room-holder (cdr (fourth room-list)))
    ;; (loop for room in room-holder
    ;; 	 )
    (values next-batch room-list)))

(defun sync-general (&key (since nil) (filter nil) (full-state nil)
		       (presence nil) (timeout nil))
  (recieve-json (concatenate 'string *homeserver* "_matrix/client/r0/sync?"
			  (when since
			    (concatenate 'string "&since=" since))
			  (when filter
			    (concatenate 'string "&filter=" filter))
			  (when full-state
			    (concatenate 'string "&full_state=" full-state))
			  (when presence
			    (concatenate 'string "&set_presence=" presence))
			  (when timeout
			    (concatenate 'string "&timeout=" timeout)))))

(defun sync-again ()
  (multiple-value-bind (batch chambers) ;;acct)
      (parse-sync (sync-general :since *sync-batch-token*))
    (setf *sync-batch-token* (cdr batch))
    ;; (setf *settings* acct ;;(update-acct acct)
    ;; 	  )
    chambers))

;; (update-*chambers* (sync-again))

(defun update-*chambers* (data)
  "lists the rooms with their updates"
  (cddr data))

;; (defun parse-sync (sync-data)
;;   "takes in the results of sync-general, and parses them."
;;   (let ((next-batch (assoc "next_batch" sync-data :test #'string=))
;; 	(rooms (assoc "rooms" sync-data :test #'string=))
;; 	(device-lists (assoc "device_lists" sync-data :test #'string=)) ;; used for encryption. 
;; 	(presence (assoc "presence" sync-data :test #'string=)) ;; ignore for now. 
;; 	(groups (assoc "groups" sync-data :test #'string=)) ;; ?
;; 	(to-device (assoc "to_device" sync-data :test #'string=)) ;; used for device management
;; 	(account-data (assoc "account_data" sync-data :test #'string=)) ;; used for storing settings regarding the user. things like notifications settings, or what rooms are direct rooms and not groups. 
;; 	(device-one-time-keys-count (assoc "device_one_time_keys_count" sync-data :test #'string=)))
;;     (values (cdr next-batch)
;; 	    (generate-room-list-init rooms)
;; 	    (cdr account-data))))

(defun parse-rooms (room-set)
  "takes a list of rooms and operates over them to parse the rooms contained 
therin."
  (let ((room-list (third room-set))
	(join nil))
    (setf join (pop room-list))
    ;;(parse-single-room (second room-list))
    room-list))

(defun generate-room-list-init (rooms)
  (mapcar #'parse-single-room (parse-rooms rooms)))

(defun parse-single-room (room &optional (old-room (make-instance 'chamber)))
  "returns an updated room, or if an old room isnt provided, a new room with
 no history"
  (let ((room-id (first room))
	(state (second room))
	(notifications (third room))
	(summary (fourth room))
	(ephemeral (fifth room))
	(timeline (sixth room))
	(account-data (seventh room))
	;; (metadata (make-instance 'chamber-current-state))
	)
    ;; (setf metadata (generate-current-state metadata state))
    (cons room-id
	  (update-room old-room `(("room-id" . ,room-id)
				  ("state" . ,state)
				  ("notifications" . ,notifications)
				  ("summary" . ,summary)
				  ("ephemeral" . ,ephemeral)
				  ("timeline" . ,timeline)
				  ("account-data" . ,account-data))))))

(defun send-json (url content &rest key-plist)
  (eval `(let ((stream (http-request ,url
				     :want-stream t
				     :method :post
				     :content-type "application/json"
				     :content ,content
				     :additional-headers
				     `(("Authorization" . ,(concatenate 'string "Bearer "
									*session-user-auth*)))
				     ,@key-plist)))
	   
	   ;; (get-json-from-stream stream :utf-8 :alist)
	   (yason:parse stream :object-as :alist))))

(defun recieve-json (url)
  (let ((stream (drakma:http-request url
				     :want-stream t
				     :method :get
				     :additional-headers
				     `(("Authorization" . ,(concatenate 'string "Bearer "
									*session-user-auth*))))))
    (yason:parse stream :object-as :alist)))

(defmacro send-json-macro (url content &rest (key-plist nil key-plist-provided-p))
  `(let ((stream (http-request ,url
			       :want-stream t
			       :method :post
			       :content-type "application/json"
			       :content ,content
			       ,(when key-plist-provided-p
				  key-plist)
			       :additional-headers
			       '(("Authorization" . ,(concatenate 'string "Bearer "
								  *session-user-auth*))))))
     (yason:parse stream :object-as :alist)))

(defmacro recieve-json-macro (url &rest (key-plist nil key-plist-provided-p))
  `(let ((stream (http-request ,url
			       :want-stream t
			       :method :get
			       ,(when key-plist-provided-p
				  key-plist)
			       :additional-headers
			       '(("Authorization" . ,(concatenate 'string "Bearer "
								  *session-user-auth*))))))
     (yason:parse stream :object-as :alist)))

