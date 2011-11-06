
@* Introduction. This document details the implementation of an API 
for the tron game used in CSCI-B351. More information about the API 
for the users can be found on the Wiki site.

The tron api is focused on providing a simple but flexible interface 
for allowing pluggable game brains or A.I.'s to play a tron game. In 
tron, two players compete against one another for space. 

{\it Need some information about the game play.}

This library provides a basic API for testing brains against one 
another as well as for connecting to a game server to play the game.

@* Some notes on terminology.
To begin, let's get some basic terminology down. We say that a given 
{\it brain} is a procedure that controls how a given tron cycle 
moves. We say that it moves in a specific compass direction from 
its current position. That is, moves are either north, west, 
south, or east. 

@p
(define valid-moves '(n w s e))

@ Furthermore, we can talk about the board on which the cycles (aka
your bot) are playing based on a coordinate system, where a given
coordinate |(x . y)| is a pair where the first value is an integer in
the range of the board's width, and represents where along the x axis
the cycle is. Specifically, a board is a row-major matrix where the
origin is in the upper left hand corner.

@* Creating a tron brain. To create a tron brain, we provide a 
syntax that handles the creation of the boiler plate for you.

\medskip\verbatim
(define-tron-brain (proc name info get play) body+ ...)
!endverbatim\medskip

\noindent The above form binds |proc| to a procedure that can be used 
to initialize a simulation.  The |proc| name will be bound to a
procedure that expects to receive a single argument, which is a port,
that allows it to communicate its moves to the driver, either locally
or remotely.

$$\.{proc} : \\{port}\to(\ \to\.{\#<void})$$

\noindent The |proc| does the initial connection and setup for the 
brain and given game instance, and returns a thunk that, when executed,
will run the brain. The brain is then expected to send its next move
to the server through the port using the |get| and |play| procedures
that are made visible to it.  The brain never needs to interact with
that port though. All the brain has to do is to use the two |get| and
|play| procedures that are bound to get the move of the opponent and
to send its move to the engine.  These two procedures have the
following syntax:

$$\eqalign{\.{get} :\ \to \\{move}||\\{game-result}\cr
\.{play} : \\{move} \to \.{\#<void>}}$$

\noindent The |info| binding points to a record about the game information, 
including the size of the board and whether you are the first or second 
player, and any obstacles that are on the board. We discuss its structure 
and how to access elements from the info structure a little later. The 
|name| should be an expression that evaluates to a string, which will 
be the name of the player that is sent to the server. 

@p
(define-syntax define-tron-brain
  (syntax-rules ()
    [(_ (proc name info get play) b1 b2 ...)
     (define (proc port)
       (let ([info @<Get board information@>]
             [get (make-get-proc port)]
             [play (make-play-proc port)]
             [name name])
         (lambda () b1 b2 ...)))]))

@ As an example of using |define-tron-brain| let's make a brain 
that randomly plays a move. Our player's name will be 
``Random move bot.''

@p
(define-tron-brain (random-move-bot "Random Move Bot" gi get play)
  (play (list-ref valid-moves (random (length valid-moves)))))

@* The game information structure. The game structure holds the 
initial board configuration, which consists of the players' 
starting positions, as well as the size of the board and the 
location of any obstacles. The |board| object will contain
the following three fields.

$$\vbox{
  \offinterlineskip
  \halign{
    \strut # & # \cr
    {\bf Field name} & {\bf Type} \cr
    \noalign{\hrule}
    $\\{starting-positions}$ & Two coordinates: player and opponent \cr
    $\\{size}$ & |(width . height)| \cr
    $\\{obstacles}$ & | ((x . y) ...)| where each coordinate is an obstacle. \cr
  }
}$$

\noindent The $\\{starting-positions}$ field will contain a pair containing 
two coordinates, and the |car| of that pair will contain the starting 
position of the player and the |cdr| the position of the opponent.

@p
(define-record-type board
  (fields
    starting-positions
    size
    obstacles))

@* Getters and ``Play''ers.
We provide some basic abstractions for creating the |get| 
and |play| procedures for our brains. Their implementations 
are discussed further below. The makers will each take a 
port to send or receive from, and will return procedures 
of the right signature for getters and players. A move
is represented as the direction from the bot's current
location to the desired location.

We first define the maker for the |get| procedure. 
The |get| procedure has the following signature:

$$\.{get} :\ \to\\{move}||\\{result}$$

@p
(define (make-get-proc port)
  (lambda () 
    (let ([res (read port)])
      (cond
        [(symbol? res) res]
        [(pair? res) res]
        ;; This should never happen, supposedly.
        [else (error #f "invalid move from opponent")]))))

@ The |play| maker is easier, because it just has to validate 
the move and then write it out. We are not assuming that the 
server or the simulator is going to handle the buffering for 
us, so we will need to flush things ourselves.

$$\.{play} : \\{move}\to\.{\#<void>}$$

\noindent On the less simple side, we want to also have the 
thunk |current-position| which will return our current position. 
We will therefore return two values from |make-play-proc| and 
we will have some hidden state that we use to track the current
position of the player on the board. This way, the user will 
easily be able to determine what the current position of 
the player is at every turn. 

@p
(define (make-play-proc port start-pos)
  (let ([pos start-pos])
    (values
      (lambda () pos)        
      (lambda (move) 
        (assert (valid-move? move))
        (write move port)
        (flush-output-port port)
        (case move
          [(n) (set! pos (cons (car pos) (-1+ (cdr pos))))]
          [(w) (set! pos (cons (-1+ (car pos)) (cdr pos)))]
          [(e) (set! pos (cons (1+ (car pos)) (cdr pos)))]
          [(s) (set! pos (cons (car pos) (1+ (cdr pos))))])))))

@* Playing a game. To play a game locally, without having a connection 
to a server or anything like that, you use the |play-tron| procedure.
It expects to receive two brains as defined by |define-tron-brain|. 
It will simulate the game and show the progress of the game on the terminal. 

$$\.{play-tron} : \\{brain1}\times\\{brain2}\to\.{\#<void>}$$

\noindent The |play-tron| procedure will take care of a few steps. Firstly, 
it needs to setup a communication layer so that the two brains can 
communicate over the same protocol as that used by the server. 

\medskip
\item{1.} Setup pseudo-server.
\item{2.} Run brains to get initialization done and get the brain thunks.
\item{3.} Play the game.
\item{4.} Print the result of the game.
\medskip

\noindent

@p
(define (play-tron board b1 b2)
  (let loop ([board board]) 
    (unless (winner? board)
	    (let ([b1-move (b1)]
		  [b2-move (b2)])
	      (loop (update-board board b1-move b2-move))))))

@ create-board takes the width, height, and a list of all the coordinates
(row,col) where the obstacles are located. 

$$\.{create-board} : \\{width}\times\\{height}\\times\\{obstacles}\to\.{\#<void>}$$

@p
(define (create-board w h obstacles)




@* Running on a server.

@* The tron server protocol.
