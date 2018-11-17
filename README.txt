This is the stub README.txt for the "matrix-framework" project.

This is a framework for working with matrix, with the purpose of
building a client.

to use this, youll have to get your user auth token, and assign
that to *session-user-auth*. This can be obtained via
(login "username" "password"). 

to get started, run (initialize), which will populate the variable
*chambers* with room data. you can look at timelines with
(parse-all-timelines (get-room-timelines-from-*chambers*)). This will
return a list formed like so: '((room-id (messages))
       	      	     	      	(room-id (messages)))
which should be ready for printing. the messages that havent been
implemented will print "this message type has not been implemented"
as a placeholder


everything is very rough at this point (16/11/2018) and nothing is set in stone,
functions may change, or made obsolete. 
