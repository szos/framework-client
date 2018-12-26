(in-package :matrix-framework)

(defun smart-login ()
  (if *session-user-auth*
      (progn
	(princ (format nil "~a is already logged in."
		       *user-address*))
	(terpri))
      (progn
	(princ "login")
	(terpri)
	(looped-login))))

(defun looped-login ()
  "prompts the user for username and password, and attempts to log in using those
credentials. if it fails we loop back and try again, otherwise we return :success"
  (loop
     do
       (let ((un nil)
	     (pw nil)
	     (login? nil))
	 (princ "username:  ")
	 (setf un (read-line))
	 ;;(terpri)
	 (princ "password:  ")
	 (setf pw (read-line))
	 ;;(terpri)
	 (setf login? (login un pw))
	 (if (eq login? :invalid-credentials)
	     (progn
	       (princ "Login failed, try again")
	       (terpri))
	     (progn
	       (princ (cat login? " successfully logged in."))
	       (terpri)
	       (terpri)
	       (return-from looped-login :success))))))


