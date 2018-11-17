This is the stub README.txt for the "matrix-framework" project.

This is a framework for working with matrix, with the purpose of
building a client.

to use this, youll have to get your user auth token, and assign
that to *session-user-auth*. This can be obtained via
(login "username" "password"). Currently, it can only log in to
the matrix homeserver. the client has a device id of SBCLclient.

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


example usage from the repl:

matrix-framework> (initialize)
matrix-framework> (parse-all-timelines (get-room-timelines-from-*chambers*))
wait some time
matrix-framework> (initialize)
matrix-framework> (parse-all-timelines (get-room-timelines-from-*chambers*))
wait some time

in this manner one can use this to monitor whats happening in all the rooms.
next steps are to build methods to select a room, and then parse just that room.
also on the docket is to implement a refresh mechanism. this is partially done via
(sync-again) but it lacks any way of integrating into *chambers*. finally, also
needed is a way to refresh single rooms, without refreshing everything. the end
game there is to have a loop constantly running when a user is 'in' a room,
so that any new events are detected asap, while general syncing is limited to
either A) when the user requests it, or B) on a much slower timer. 

This will require some sort of timer,
hopefully something suitable is available on quicklisp. if not, look to the
stumpwm timers implementation. 
