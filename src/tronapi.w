
@* Introduction. This document details the implementation of an API
for the tron game used in CSCI-B351. More information about the IU
Tron game can be found at the following web address:

\medskip\verbatim
http://www.sacrideo.us/iu-tron/
!endverbatim\medskip

\noindent 
The tron api is focused on providing a simple but flexible interface
for allowing pluggable game brains or A.I.'s to play a tron game. In
tron, two players compete against one another for space. Each player
drives a virtual light cycle that continuously flies inside of a
virtual map (2-dimensional) at a constant rate of speed. You can turn
your cycle left and right through the map, trying to stay alive longer
than your opponent. The walls are deadly, and hitting any wall or
protrusion results in instant death. Additionally, your light cycles
are emitting light that leaves a solidified trail behind, and hitting
these walls, either your own or the trail left by your opponent also
results in instant death. Don't die first! You can either win the game
by being alive longer than your opponent, lose by dying first, or you
can draw by dying at the exact same time.

In this library, each cycle moves in discrete lock-step fashion, where
the players each take a turn at the same time, moving their cycle
forward one step. Since the most interesting aspects of the game are
the actual brains that drive the cycles, this library provides all of
the basic functionality for creating tron brains.  Here's a simple
list of the features that are provided:

{\narrower
\medskip
\item{1.} You can define brains that handle the server protocol 
transparently, and use them both locally on your own machine 
and remotely to play on the server.
\item{2.} You can run your brain against others locally, so that 
you can test your brains offline without having to connect to the 
game server for each run.
\item{3.} You can easily run your brain against a remote server 
that you specify.\par}

@ {\it Notes on terminology.}  To begin, let's get some basic
terminology down. We say that a given {\it brain} is a procedure that
controls how a given tron cycle moves. We say that a brain |plays| a
given move, which is a direction either north, south, east, or
west. To move back against the way that you came results in instant
death, and you must make a move at every turn.

We will define a variable |valid-moves| here which will give you 
the set of your valid moves.

@p
(define valid-moves '(n w s e))

@ The board on which the cycles are playing is a standard 2
dimensional grid that has $[0,w)$ columns and $[0,h)$ rows, where $h$
is the height of the map and $w$ is the width. The origin is in the
upper lefthand corner. We refer to a given grid by coordinate point in
the form |(x . y)| where |x| is the column and |y| is the row.

The game is played on a torus board, meaning that if you go off the
edge of the board on one side, you appear on the opposite side of the
board going the same direction.

@ Every game starts in the same way, there is a map, and then there
are starting positions for each of the players. Each player is given
the starting position of the other player, as well as the size of the
map and the list of which coordinates in the map are walls. This means
that both players have perfect information of the playing field when
they start.

Each cycle's state on the board is in the form of a location and a 
direction. This all comes together to form the cycle's position. 
A position is encoded as a pair of coordinate and direction or 
|((x . y) . dir)| where |dir| is a member of |valid-moves|. 

@* Creating a tron brain. To create a tron brain, we provide a 
syntax that handles the creation of the boiler plate for you.

\medskip\verbatim
(define-tron-brain (proc (name size walls play ppos opos) 
                         (state init))
  body+ ...)
!endverbatim\medskip

\noindent Here is a short synopsis of the above identifiers:

$$\vbox{
  \offinterlineskip
  \halign{
    \strut # & # \hfill\cr
    {\bf Identifier} & {\bf Use} \cr
    \noalign{\hrule}
    $\\{proc}$  & Name of brain procedure \cr
    $\\{name}$  & String of player name \cr
    $\\{state}$ & Variable to hold user provided state \cr
    $\\{init}$  & Initial state expression \cr
    $\\{size}$  & Holds the size of the map \cr
    $\\{walls}$ & Alist of coordinates for walls \cr
    $\\{ppos}$  & Player position \cr
    $\\{opos}$  & Opponent position \cr
    $\\{play}$  & Procedure to play move \cr
    $\\{body+}$ & One or more expressions for the brain \cr
  }
}$$


\noindent The above form binds |proc| to a procedure that can be used 
to initialize a simulation.  The |proc| name will be bound to a
procedure that expects to receive two arguments, which are ports,
that allow it to communicate its moves to the driver, either locally
or remotely.

$$\.{proc} : 
  \\{play-port}\times\\{info-port}
  \to(\\{port}\times\\{state}\to\.{\#<void>})$$

\noindent The value returned by the |proc| procedure is another procedure 
which is the main brain procedure. Whenever we call this procedure, we
expect that the user provided code will run, and that a single play
will be made. We want to make sure that we accept the user state at
the beginning of each turn from the previous turn, and we want to
provide the port that we are going to use to get moves from the
server. We use a port here instead of passing |ppos| and |opos|
directly because we need to be able to work both on the remote server
as well as a local one.  The |proc| procedure is thus in charge of
doing the initial protocol negotiation and getting all of the static
information set up before returning the brain playing procedure. The
user's brain code should send the next move to the server using the
|play| procedure. This means that the user body brain code never has
to worry about things like ports and the like.  The |play| procedure
has the following signature:

$$\.{play} : \\{move}\to\.{\#<void>}$$

\noindent It may be that the player or brain needs to keep some information 
around each time that it is called for the next time around. In order
to do this, simply return that state as the return value of the brain
body code and it will be available to you again in the |state|
variable the next time that the brain is called.

The |name| element should be an expression that evaluates into a
string that will be used as the name of the player on the server when
reporting scores and information. The |size| variable is a pair 
|(w . h)| containing the width and height of the map. The |walls|
variable will contain an association list of the form |((x . y) ...)|
that gives the locations of each wall on the map. The variables |ppos|
and |opos| both have the form |(x . y)| and represent the current
positions of the player and opponent respectively.

@ Let's start by defining the actual |define-tron-brain| syntax. We
want to expand into something that will implement all of the above
semantics, though some of the work will be left until later.

@p
(define-syntax define-tron-brain
  (syntax-rules ()
    [(_ (proc (name size walls play ppos opos) (state init)) b1 b2 ...)
     (define (proc play-port info-port)
       @<Send |name| and get game |size| and |walls|@>
       (let ([play (make-play-proc play-port)]
             [state init])
         (lambda (port)
           @<Get player and opponent positions, |ppos| and |opos|@>
           (let-values ([(newstate) (let () b1 b2 ...)])
             (set! state newstate)))))]))

@ The |play| procedure is actually a simple closure over the
|play-port|.  Recall the signature for the |play| procedure.

$$\.{play} : \\{move} \to \.{\#<void>}$$

\noindent We can then easily define |make-play-proc| as a |play| procedure 
generator.

@p
(define (make-play-proc port)
  (lambda (move)
    (assert (memq move valid-moves))
    (format port "~s~n" move)
    (flush-output-port port)))

@ As an example of using |define-tron-brain| let's make a brain that
randomly plays a move. Our player's name will be ``Random move bot.''
Note that this bot very well may pick a move that sends it backwards,
thus dieing and losing. ``Random move bot'' is not very smart.

@p
;;; This example is out of date, fix it. XXX
(define-tron-brain 
  (random-move-bot ("Random Move Bot" size walls play ppos opos) (state #f))
  (let* ([adjacent-moves (map get-pos valid-moves (make-list 4 p))]
	 [safe? (lambda (p) (not (member p walls)))]
	 [safe-moves (filter safe? (adjacent-moves ppos))])
    (play (list-ref safe-moves (random (length safe-moves))))))

@* The Tron Server Protocol. The general process of a client making 
a connection to the tron server can be outlined thus:

{\medskip\narrower
\item{1.} Client sends player name.
\item{2.} Server sends the size of the map as |(w . h)|.
\item{3.} Server sends the walls as an a list |((x . y) ...)|.
\item{4.} Server sends the positions of the cycles, listing first the 
player and then the opponent.
\item{5.} Player sends move in the form of a direction.
\item{6.} If the game is won or a draw, the server sends |win| to 
the winner, and |loss| to the loser or |draw| to both in the case of 
a draw.
\item{7.} Otherwise, repeat starting at step 4.\par\medskip}

\noindent 

@* Playing a game. To play a game locally, without having a connection
to a server or anything like that, you use the |play-tron| procedure.
It expects to receive a board specification and two brains as defined
by |define-tron-brain|.  It will simulate the game and show the
progress of the game on the terminal.

$$\.{play-tron} : \\{board}\times\\{brain1}\times\\{brain2}\to\.{\#<void>}$$

\noindent The |play-tron| procedure will take care of a few steps. Firstly, 
it needs to setup a communication layer so that the two brains can
communicate over the same protocol as that used by the server.

\medskip
\item{1.} Setup pseudo-server.
\item{2.} Run brains to get initialization done and get the brain thunks.
\item{3.} Play the game.
\item{4.} Print the result of the game.
\medskip

\noindent We use string ports to get the data from the players, since we 
can use the getters on R6RS string ports to constantly get string values 
of the responses that brains make without having to recreate brains.

@p
(define (play-tron size walls pos1 pos2 b1 b2)
  (assert (valid-size? size))
  (assert (valid-walls? walls))
  (assert (for-all valid-position? (list pos1 pos2)))
  (assert (for-all procedure? (list b1 b2)))
  (let-values ([(b1-play-port b1-get) (make-string-output-port)]
               [(b2-play-port b2-get) (make-string-output-port)])
    (let ([info-port (make-info-port size walls)])
      (let ([b1-play (b1 b1-play-port info-port)]
            [b2-play (b2 b2-play-port info-port)])
        @<Simulate tron game@>))))

@ Because we are simulating the actual behavior of the server, we are 
using a number of different ports, where the real server might use 
only one for all of the communication. The first of these is our 
information port. This port should contain the board size and the 
wall information in the same format as the server will send it. In 
this case, this means just using |write| a few times.

@p
(define (make-info-port size walls)
  (make-string-input-port
    (format "~s~s~n" size walls)))

@ The tron game proceeds from the protocol above starting at step 4. 
We send out the the player and opponent positions and then we need 
to get the moves back from each player. 

@c (pos1 pos2 size walls b1-play b2-play b1-get b2-get)
@<Simulate tron game@>=
(let loop ([pos1 pos1] [pos2 pos2] [walls walls])
  (let ([port1 (make-pos-port pos1 pos2)]
        [port2 (make-pos-port pos2 pos1)])
    (b1-play port1) (b2-play port2)
    (let ([m1 (string->move (b1-get))]
          [m2 (string->move (b2-get))])
      @<Print game position@>
      @<Get new positions and walls@>
      (let ([status (game-status new-pos1 new-pos2 new-walls)])
        (case (game-status new-pos1 new-pos2 new-walls)
          [(draw p1 p2) @<Print game result@>]
          [else (loop new-pos1 new-pos2 new-walls)]))))

@ Position ports encode the first part of the fourth protocol stage, where 
we send the player positions to each player. In this case, we assume 
that the first argument is the first player to send, and the second argument 
the opponent and second position to send.

@p
(define (make-pos-port p1 p2)
  (make-string-input-port 
    (format "~s~s~n" p1 p2)))

@ The procedure |string->move| should take a single string 
representing the play made by a player in the syntax of the 
server protocol. That is, it should be a symbol that is a 
valid member of |valid-moves|. 

@p
(define (string->move str)
  (let ([val (with-input-from-string str read)])
    (or (and (symbol? val) (member val valid-moves) val)
        (error #f "Not a valid move" val))))

@ To track the game, it is nice if we are able to print the position 
of the players on the board. To do this, we'll print the current game
board out on the terminal.

@c (size walls)
@<Print game position@>=
(let ol ([i 0])
  (unless (= i (cdr size))
    (let il ([j 0])
      (unless (= j (car size))
        (if (member (cons j i) walls)
            (printf "# ")
            (printf ". "))
        (il (1+ j))))
    (newline)
    (ol (1+ i))))

@ Once we have the moves that the players have made, we want to make 
those positions and create the updated positions values. We also 
want to make sure that the walls of the current positions of the 
players are thrown into the walls set. 

@c (size pos1 pos2 m1 m2 walls) => (new-pos1 new-pos2 new-walls)
@<Get new positions and walls@>=
(define width (car size))
(define height (cdr size))
(define new-pos1 (get-pos m1 pos1))
(define new-pos2 (get-pos m2 pos2))
(define new-walls (cons* (car pos1) (car pos2) walls))

@ We provide the |get-pos| procedure, which takes a move or direction 
and an old position, and returns a new position assuming that we moved
in that new direction. 

$$\.{get-pos} : \\{move}\times\\{position}\to\\{new-position}$$

@p
(define (get-pos m old)
  (let ([x (caar old)] [y (cdar old)])
    (cons
      (case m 
        [(n) (cons x (mod (-1+ y) height))]
        [(w) (cons (mod (-1+ x) width) y)]
        [(e) (cons (mod (1+ x) width) y)]
        [(s) (cons x (mod (1+ y) height))]
        [else (error #f "invalid move" m)])
      m)))

@ The |game-status| procedure takes the position of player1, position
of player2, and the current list of walls. If either player position
is the same as a wall, that player has crashed. If both players have
crashed, the game is a draw. If one player has crashed then the other
player has won. If neither player has crashed let the game continue!

@p
(define (game-status pos1 pos2 walls)
  (let ([p1-dead? (member (car pos1) walls)]
	[p2-dead? (member (car pos2) walls)])
    (cond
     [(and p1-dead? p2-dead?) 'draw]
     [p1-dead? 'p2]
     [p2-dead? 'p1]
     [else 'not-over]))

@ When the game has concluded, we want to print the 
results of the win. We will print out nice little 
messages for people here.
We also want to make sure that we send the correct 
states to each player.

@c (status) 
@<Print game result@>=
(case status
  [(draw) (printf "The game was a draw.~n")]
  [(p1) (printf "Congratulations player 1, you won.")]
  [(p2) (printf "Congratulations player 2, you won.")]
  [else (error #f "invalid status" status)])


  @* Running) on a server.
