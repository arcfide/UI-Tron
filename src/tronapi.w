
@* Introduction. This document details the implementation of an API 
for the tron game used in CSCI-B351. More information about the 
IU Tron game can be found at the following web address:

\medskip\verbatim
http://www.sacrideo.us/iu-tron/
!endverbatim\medskip

\noindent 
The tron api is focused on providing a simple but flexible interface for 
allowing pluggable game brains or A.I.'s to play a tron game. In
tron, two players compete against one another for space. Each player 
drives a virtual light cycle that continuously flies inside of a virtual 
map (2-dimensional) at a constant rate of speed. You can turn your 
cycle left and right through the map, trying to stay alive longer than 
your opponent. The walls are deadly, and hitting any wall or protrusion 
results in instant death. Additionally, your light cycles are emitting 
light that leaves a solidified trail behind, and hitting these walls, 
either your own or the trail left by your opponent also results in 
instant death. Don't die first! You can either win the game by being 
alive longer than your opponent, lose by dying first, or you can 
draw by dying at the exact same time.

In this library, each cycle moves in discrete lock-step fashion, 
where the players each take a turn at the same time, moving their 
cycle forward one step. Since the most interesting aspects of the 
game are the actual brains that drive the cycles, this library 
provides all of the basic functionality for creating tron brains.
Here's a simple list of the features that are provided:

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

@ {\it Notes on terminology.} 
To begin, let's get some basic terminology down. We say that a given 
{\it brain} is a procedure that controls how a given tron cycle 
moves. We say that a brain |plays| a given move, which is a direction 
either north, south, east, or west. You cannot move back against 
the way that you came, and you must make a move at every turn.

We will define a variable |valid-moves| here which will give you 
the set of your valid moves.

@p
(define valid-moves '(n w s e))

@ The board on which the cycles are playing is a standard 2 dimensional 
grid that has $[0,w)$ columns and $[0,h)$ rows, where $h$ is the height 
of the map and $w$ is the width. The origin is in the upper lefthand 
corner. We refer to a given grid by coordinate point in the form |(x . y)| 
where |x| is the column and |y| is the row. 

@ Every game starts in the same way, there is a map, and then there 
are starting positions for each of the players. Each player is given 
the starting position of the other player, as well as the size of the 
map and the list of which coordinates in the map are walls. This means 
that both players have perfect information of the playing field when 
they start.

@* Creating a tron brain. To create a tron brain, we provide a 
syntax that handles the creation of the boiler plate for you.

\medskip\verbatim
(define-tron-brain (proc name state size walls ppos opos play) body+ ...)
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
procedure that expects to receive a single argument, which is a port,
that allows it to communicate its moves to the driver, either locally
or remotely.

$$\.{proc} : 
  \\{port}\to(\\{state}\times\\{ppos}\times\\{opos}\to\.{\#<void>})$$

\noindent The |proc| does the initial connection and setup for the 
brain and given game instance, and returns a procedure that, when executed,
will run the brain. The brain is then expected to send its next move
to the server through the port using the |play| procedure.  
The brain never needs to interact with that port though. 
All the brain has to do is to use |play| to send its move to the server.
The |play| procedure has the following signature:

$$\.{play} : \\{move} \to \.{\#<void>}$$

\noindent It may be that the player or brain needs to keep some information 
around each time that it is called for the next time around. In order to 
do this, simply return that state as the return value of the brain body 
code and it will be available to you again in the |state| variable the 
next time that the brain is called.

The |name| element should be an expression that evaluates into a string 
that will be used as the name of the player on the server when reporting 
scores and information. The |size| variable is a pair |(w . h)| containing 
the width and height of the map. The |walls| variable will contain 
an association list of the form |((x . y) ...)| that gives the locations 
of each wall on the map. The variables |ppos| and |opos| both have the 
form |(x . y)| and represent the current positions of the player and 
opponent respectively. 

@p
;;; This is currently broken and doesn't match the spec above, 
;;; so it must be fixed.
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
