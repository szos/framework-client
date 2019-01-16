(in-package :matrix-framework)

;;; this file is for holding our custom repl.

(defparameter *allowed-commands*
  '(logout initialize quit exit grab-single-room view-room inspect-room-state list-rooms
    exit-test-repl print-commands view-room*
    ))

(defun print-commands (&optional (commands *allowed-commands*))
  (loop for command in commands
     do
       (print command)))

;;; OK! look at clim-test.lisp in ~/, as it will have an example clim window/app thingy.
;;; we are going to use clim for our gui here. it can work! Were going to build the
;;; window similar to Riot, divided (horizontally ()
;;;                                   (1/4 client-info+rooms-list)
;;;                                   (1/2 (vertically ()
;;;                                          (3/4 room-info)
;;;                                          (1/4 interactor))
;;;                                   (1/4 room-info))

(defparameter *permitted-variables*
  '(*room-name->id*))

(defparameter *repl-persist?* t)

(defun test-repl ()
  (setf *repl-persist?* t)
  (smart-login)
  (loop while *repl-persist?* do
       (test-print (test-eval (test-read)))))

(defun command? (cmd ;&optional ()
			   )
  (if (member cmd *allowed-commands*)
      t
      nil))
;;; we can say, if the room name event isnt found, we search for the members,
;;; and name the room after the first 4 members. 

(defun test-read ()
  (princ "command:  ")
  (let ((cmd (read-from-string ;; lets us input commands without parens.
	      (let ((input (read-line)))
		(cat "(" input ")")      ;; just for now, we let this be used to
		))))
    (cond ((or (equal cmd '(quit)) (equal cmd '(exit)))
	   (logout)
	   (princ "logging out and exiting application")
	   (terpri)
	   '(quit))
	  (t
	   cmd))))

(defun quit ()
  (logout)
  (setf *repl-persist?* nil))

(defun exit-test-repl ()
  (setf *repl-persist?* nil))

(defun test-eval (form)
  (cond ((member (car form) *permitted-variables*)
	 (eval (car form)))
	((and (member (car form) *allowed-commands*) (eq (car form) 'view-room)
	      (not (stringp (second form))))
	 (let ((room-name ""))
	   (loop for sym in (rest form)
	      do (setf room-name (cat room-name " " (format nil "~A" sym))))
	   ;; (eval (cons (car form) (subseq room-name 1)))
	   (eval (list (car form) (subseq room-name 1)))))
	((member (car form) *allowed-commands*)
	 (eval form))
	(t
	 "Form Not Permitted"))
  ;; (let* ((cmd (first form))
  ;; 	 (args (rest form))
  ;; 	 ;; we can say if the arg is in the list of allowed commands OR permitted
  ;; 	 ;; variables, then unquote it and add () if its a command. then we can
  ;; 	 ;; say if, once its added a (, it will search for a ) symbol in the
  ;; 	 ;; following args. if it finds one, it treats it as the closing paren.
  ;; 	 ;; if it doesnt find one, it adds the paren at the end. this will all
  ;; 	 ;; be in the mapcar. perhaps a (let () (loop for...)) instead...
  ;; 	 (eval-able-args nil))
  ;;   (when (stringp args)
  ;;     (mapcar #'(lambda (arg)
  ;; 		  (if (symbolp arg)
  ;; 		      `',arg
  ;; 		      arg))
  ;; 	      args))
  ;;   (cond ((member cmd *permitted-variables*)
  ;; 	   (eval cmd))
  ;; 	  ((member cmd *allowed-commands*)
  ;; 	   (eval (cons cmd eval-able-args)))
  ;; 	  (t
  ;; 	   "Form Not Permitted")))
  )

(defmacro quoter (thing-to-quote)
  `(quote ,thing-to-quote))

(defun test-print (value)
  (when value (princ value))
  (terpri))

(defun room-name->id (name)
  (cdr (assoc name *room-name->id* :test #'string=)))

(defun view-room (&rest name)
  "displays a rooms formatted list of messages. the name can be a symbol or a string. 
eventually we will have to work in some sort of syntax... perhaps command arg arg arg...
and move the args into their own thing and detect if they're quoted or if theyre 
intended to be called as arguments in their own right... for now we will stick with 
quoting the args to keep them as data. 
OR BETTER YET: we could treat it all as data and quote everything unless it is of the
form (:eval (lisp s expressions)) which would get evaluated before being put into the 
main eval statement, or perhaps just spliced in..."
  ;; (unless (stringp (car name))
  ;;   (setf name (let ((room-name ""))
  ;; 		 (mapcar #'(lambda (name-as-symbol)
  ;; 			     (setf room-name (cat room-name (symbol-name name-as-symbol))))
  ;; 			 name)
  ;; 		 (print room-name)
  ;; 		 room-name)))
  (if (stringp (car name))
      (display-room-events-list
       (parse-timeline-events
	(get-events-from-timeline (get-timeline-from-room (grab-single-room (car name))))))
      (setf name (let ((room-name ""))
		   (mapcar #'(lambda (name-as-symbol)
			       (setf room-name (cat room-name (symbol-name name-as-symbol))))
			   name)
		   (print room-name)
		   room-name))))

(defun view-room* (name)
  "name should be a string. it returns a list of
events that have been formatted. "
  (parse-timeline-events
   (get-events-from-timeline (get-timeline-from-room (grab-single-room name)))))

(defun set-up-rooms ()
  (print "Syncing rooms...")
  (initialize)
  ())

(defun send-message-to (room-name message)
  (send-message-to-room (room-name->id room-name) message))

(defun print-timeline (name)
  "this is a duplicate of view-room..."
  (parse-timeline-events
   (cdr (assoc "events" (cdr (assoc "timeline" (cdr (grab-single-room name))
				    :test #'string=))
	       :test #'string=))))

(defun inspect-room-state (name)
  "takes a room, and searches for the state portion, and then the events portion
within the state. it then formats and prints those events. "
  (parse-timeline-events
   (cdr (assoc "events" (cdr (assoc "state"
				    (cdr (grab-single-room name))
				    :test #'string=))
	       :test #'string=))))

(defun send-message-to-room (room-id message)
  "this can be implemented using dynamic scope! we define a dynamic variable
*current-room*, and on the first call enter a room, we wrap it all in a let,
which sets *current-room* to the room thats being visited/watched. this works
because dynamic variables and lexical scope are preserved through function 
calls, so we can just 'let' it in the first function call, and dont have to 
worry about it for anything below there, as each call will have no effect. 
see the parameter *test-val* and the associated functions at the bottom of
this file.  this lets us not send the room in the function call, via a 
&optional \(room *current-room*\)  "
  (let ((url (cat *homeserver* "_matrix/client/r0/rooms/"
		  room-id "/send/m.room.message?"))
	(content (format nil "{\"msgtype\":\"m.text\", \"body\":~S}" message)))
    (send-json-macro-style url content)))

;; (defun inspect-room-state (name)
;;   (display-room-events-list
;;    (parse-timeline-events
;;     (get-events-from-timeline
;;      (cdr (assoc "state" (cdr (grab-single-room name)) :test #'string=))))))

;;; heres a list of all the things I want to implement for top level usage.
(defparameter *current-room* nil)

(defun gen-spaces (number-of-spaces)
  (cat (loop for x from 1 to number-of-spaces
	    collect #\SPACE)))

(defun names-formatter (name-list &optional (counter 0))
  "return the names, formatted in a list. it adds spaces to the names list. "
  (if name-list
      (let ((len (length (car name-list)))
	    (result nil))
	(if (> len counter)
	    (multiple-value-bind (names length)
		(names-formatter (cdr name-list) len)
	      (when (> length len)
		(setf len length))
	      (setf counter len)
	      (setf result names))
	    (multiple-value-bind (names length)
		(names-formatter (cdr name-list) counter)
	      (when (> length counter)
		(setf counter length))
	      (setf result names)))
	;; at this point, counter is the max it will be. 
	(values
	 (cons (cat (car name-list) (gen-spaces (- (+ counter 4) len)))
	       result)
	 counter))
      (values nil 0)))



(defun list-rooms (&optional (rooms *room-name->id*))
  (let ((names-to-print (names-formatter (mapcar #'car rooms))))
    (if (> (length (car names-to-print)) 40)
	;;over 40 one per line
	(loop for name in names-to-print
	   do
	     (print name))
	(progn
	  (setf names-to-print (churn-2x names-to-print))
	  (loop for name-s in names-to-print
	     do
	       (print name-s))))))

(defun churn-2x (list-of-names)
  (when list-of-names
    (cons (cat (first list-of-names) (second list-of-names))
	  (churn-2x (cddr list-of-names)))))

(defun show-room (room-name/id)) 
(defun watch-room (room-name/id)
  "this could go like show room -> poll for update -> if found show room, 
otherwise poll for update again.")
;; watch-room uses show room, but continually polls for more information from
;; the server, and when it gets some, it updates. but if a poll results in
;; nothing new, no updates, no reworking, no nothing is run. save them runtime
;; resources.
(defun expand-room-history (room-name/id)
  "this will use the rooms previous batch token to retrieve more messages. 
it adds them to the room store, in the appropriate place, and ")

(defun create-room ())
(defun invite-user-to-room (user &optional (room *current-room*)))
;(defun )



;; (defparameter *test-val* "test")
;; (defun test-let-vars ()
;;   (format t "in test-let-vars, test val is ~A~%" *test-val*)
;;   (let ((*test-val* "hi"))
;;     (format t "in test-let-vars after let, test val is ~A~%" *test-val*)
;;     (test-letter)))
;; (defun test-letter ()
;;   (format t "in test-letter before let, test val is ~A~%" *test-val*)
;;   (let ((*test-val* "bye"))
;;     (format t "after let, test val is ~A~%" *test-val*)))

