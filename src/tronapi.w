\input graphicx

\def\title{TRON API (Version 1.0)}
\def\topofcontents{\null\vfill
  $$\includegraphics[width=6in]{logo_tron_black.eps}$$
  \centerline{\titlefont Tron API}
  \vskip 15pt
  \centerline{(Version 1.0)}
  \vfill}
\def\botofcontents{\vfill
\noindent
Copyright $\copyright$ 2011 Aaron W. Hsu $\.{<awhsu@@indiana.edu>}$, 
Dustin Dannenhauer $\.{<dtdannen@@indiana.edu>}$.
\smallskip\noindent
Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.
\smallskip\noindent
THE SOFTWARE IS PROVIDED ``AS IS'' AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
}

\font\tt = "APL385 Unicode" at 10pt

@* Introduction. This document details the implementation of an
API for the tron game used in CSCI-B351. The tron api is focused on
providing a simple but flexible interface for allowing pluggable game
brains or A.I.'s to play a tron game. In tron, two players compete
against one another for space. Each player drives a virtual light
cycle that continuously flies inside of a virtual map (2-dimensional)
at a constant rate of speed. You can turn your cycle left and right
through the map, trying to stay alive longer than your opponent. The
walls are deadly, and hitting any wall or protrusion results in
instant death. Additionally, your light cycles are emitting light
that leaves a solidified trail behind, and hitting these walls,
either your own or the trail left by your opponent also results in
instant death. Don't die first! You can either win the game by being
alive longer than your opponent, lose by dying first, or you can draw
by dying at the exact same time.

In this library, each cycle moves in discrete lock-step fashion, where
the players each take a turn at the same time, moving their cycle
forward one step. Since the most interesting aspects of the game are
the actual brains that drive the cycles, this library provides all of
the basic functionality for creating tron brains.  Here's a simple
list of the features that are provided:

{\narrower\medskip
\item{1.} You can define brains that handle the server protocol 
transparently, and use them both locally on your own machine 
and remotely to play on the server.
\item{2.} You can run your brain against others locally, so that 
you can test your brains offline without having to connect to the 
game server for each run.
\item{3.} You can easily run your brain against a remote server 
that you specify.\par\medskip}

\noindent
Every game starts in the same way, there is a map, and then there are
starting positions for each of the players. Each player is given the
starting position of the other player, as well as the size of the map
and the list of which coordinates in the map are walls. This means
that both players have perfect information of the playing field when
they start.

@ {\it The Board Representation.} The board on which the cycles are
playing is a standard 2 dimensional grid that has $[0,w)$ columns and
$[0,h)$ rows, where $h$ is the height of the map and $w$ is the
width. The origin is in the upper lefthand corner. We refer to a given
grid by coordinate point in the form |(x . y)| where |x| is the column
and |y| is the row. The game is played on a torus board, meaning that
if you go off the edge of the board on one side, you appear on the
opposite side of the board going the same direction. Each cycle's
state on the board is recorded as a coordinate in the above plane. A
position is a pair |(x . y)| where |x| is the horizontal position of
the cycle (or column) and |y| is the vertical (or row) position of the
column. The indexes |x| and |y| are zero-indexed.

You can move on a board in any of the major compass direction. That is, 
you can move north, south, east, or west. You cannot move diagonally 
in a single step. The variable |valid-moves| is a list containing 
the valid directional moves in our symbolic representation.

@p
(define valid-moves '(n w s e))

@* Tron Brains. To begin, let's get some basic terminology down. At
the heart of this library is the concept of a tron {\it brain}. A
tron brain can be thought of as the artificial intelligence that will
drive a specific light cycle. A tron brain has one and only one job,
to decide what the next move to make, which is really what the next
direction should be. We use a special syntax called |define-tron-brain|
to actually create the brains. This syntax handles all of the heavy
lifting in the background so that the code the user (that's you)
has to write is limited to deciding the right move to make. 

Since most people like to start with an example, let's consider one.
As an example of using |define-tron-brain| let's make a brain
that randomly plays a move. Our brain's name will be ``Random move
bot.'' This bot is smart enough to not run into any walls if it doesn't
have to. On the other hand, if it doesn't have a safe move that it
can make, it will choose a move to make at random from among all of
the possible valid moves, even though it knows that none of them are
safe, because you always must send a move to the server, even if it
is a bad one.  Pay attention to our use of |(walls orig-walls)| which
sets up our state to be |orig-walls| initially. On each iteration,
we update |walls| implicitly by returning the new value we want to
be held by |walls|. We use this to keep track of the trails left
behind by the cycles as well as the original walls. We will talk
about this more in further sections when we talk about the syntax of
|define-tron-brain| in detail.

@p
@<Define |define-tron-brain|@>
(define-tron-brain (random-move-bot 
                     ("Random Move Bot" size orig-walls play ppos opos) 
                     (walls orig-walls))
  (let ([new-walls (cons* ppos opos walls)])
    (define (safe? m) 
      (and (not (member (get-pos m ppos size) new-walls)) #t))
    (let ([safe-moves (filter safe? valid-moves)])
      (play 
        (if (null? safe-moves)
            (list-ref valid-moves (random (length valid-moves)))
            (list-ref safe-moves (random (length safe-moves)))))
      new-walls)))

@ Here's an example of the random brain in action, using the 
printed representation from |play-tron|, which is described 
later. Here, the first player is represented by |⍋| and has 
won the game, whereas the |⍣| is the crashing, smoldering 
remains of what was player 2 (|⍋|). The dominos |⌹| are what 
is left over from the trail of their cycles.

\medskip\verbatim
                   ⍣ ⌹ ⌹ ∘ ⌹ ⌹ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ 
                   ∘ ⌹ ⌹ ⌹ ⌹ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ 
                   ⌹ ⌹ ⌹ ⌹ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ 
                   ⌹ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ 
                   ⌹ ⌹ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ 
                   ∘ ⌹ ⌹ ⌹ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ 
                   ∘ ⌹ ⌹ ⌹ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ 
                   ⌹ ⌹ ⌹ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ 
                   ⌹ ⌹ ⍋ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ 
                   ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ 
                   ∘ ∘ ⌹ ⌹ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ 
                   ∘ ∘ ⌹ ⌹ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ 
                   ∘ ∘ ⌹ ⌹ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ 
                   ∘ ∘ ⌹ ⌹ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ 
                   ⌹ ⌹ ⌹ ⌹ ⌹ ⌹ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ⌹ 
                   ⌹ ⌹ ⌹ ∘ ∘ ⌹ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ∘ ⌹ 
!endverbatim

@* 2 Creating brains. The syntax for defining a tron brain is as 
follows:

\medskip\verbatim
(define-tron-brain (proc (name size walls play ppos opos) 
                         (state init))
  body+ ...)
!endverbatim\medskip

\noindent Now, there are quite a few identifiers there, so it's helpful 
to start with what the user would normally write. Basically, the 
|body+ ...| code is the code that will actually be run each time that 
a brain is needed to get a specific move for a cycle. That is, if we are 
simulating a game locally using |play-tron| (discussed later), then 
every time we need to know what the next move is going to be, the 
|play-tron| procedure will evaluate the |body+ ...| code. That |body+ ...|
code had better send the next move to the server setup by |play-tron|.

The |define-tron-brain| syntax provides a lot of assistant to the user 
so that most of the information that one would need to decide what move 
to make next is readily available. Let's start with a short synopsis 
of what each of the identifiers in the |define-tron-brain| syntax will 
be when we run the |body+ ...| code.

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
    $\\{walls}$ & List of coordinates for walls \cr
    $\\{ppos}$  & Player position \cr
    $\\{opos}$  & Opponent position \cr
    $\\{play}$  & Procedure to play move \cr
    $\\{body+}$ & One or more expressions for the brain \cr
  }
}$$

\noindent The |name| element should be an expression that evaluates into a
string that will be used as the name of the player on the server when
reporting scores and information. The |size| variable is a pair 
|(w . h)| containing the width and height of the map. The |walls|
variable will contain a list of the form |((x . y) ...)|
that gives the locations of each wall on the map. The variables |ppos|
and |opos| both have the form |(x . y)| and represent the current
positions of the player and opponent respectively.

The |proc| identifier that you give will be the only definition that 
is actually visible outside of the |body+ ...| code. It will be bound to 
a procedure that represents the brain you have just defined. When we talk 
about the main brain procedures or objects we are talking about the procedures 
|proc| that are defined by |define-tron-brain|. You need to make sure that
any simulator that you use such as |play-tron| that needs a brain is given 
one or more of these procedures. You can read more about |play-tron| further 
down to know exactly what it expects to be given when you run it.

We haven't yet talked about |play|. This is an interesting one. Specifically, 
|play| is a procedure that is visible in |body+ ...| that you use to send a 
move to the server. That is, if you want to send a move such as north to 
the server, you don't need to do anything fancy: all you do is evaluate 
something like |(play 'n)| before the end of |body+ ...| and you will be 
done. You should be careful to only call |play| once for each invokation of 
your brain. You shouldn't send move than one move at a time to the server.
The |play| procedure has the following signature:

$$\.{play} : \\{move}\to\.{\#<void>}$$

\noindent Now let's talk about the state.  It may be that the
brain needs to keep some information around each time that it is
called, such as a history of the moves that have already been made.
To facilitate this, we allow the user to specify a variable |state|
that will be initialized to the value of the |init| expression,
which is evaluated once for that value. Then, on each invokation of
the brain mover, when we evaluate the |body+ ...| code, we will take
the value returned by the |body+ ...| code and bind it to the |state|
variable, replacing the previous state. This means that the next time
the brain mover (an internal element of the implementation of a brain
that you don't have to worry about) is called, it will know what it
returned last time. As you saw in the example, this can be used to
keep track of the changing board state, and other interesting things.

{\it\medskip\noindent At this point, if you do not care to see how 
we implement this macro, you can skip to the next titled section.}

@ The |define-tron-brain| form binds |proc| to a procedure that can be used 
to initialize a simulation.  The |proc| name will be bound to a
procedure that expects to receive two arguments, which are ports,
that allow it to communicate its moves to the driver, either locally
or remotely.

$$\.{proc} : 
  \\{play-port}\times\\{info-port}
  \to(\\{port}\to\.{\#<void>})$$

\noindent The value returned by the |proc| procedure is another
procedure which is the main brain procedure. Whenever we call this
procedure, we expect that the user provided code will run, and that
a single play will be made and sent to the server. This procedure
handles the state itself, and so the state is not a part of the
visible interface, hence the $\.{\#<void>}$ return and the single
port argument.  The port argument should allow us to get the player
positions from the
@q XXX: A server protocol index entry should go here.
server (see the server protocol section for more details on this).
We use a port here instead of passing |ppos| and |opos| directly
because we need to be able to use the same brain on the remote
server as well as a local one.  The |proc| procedure does the initial
protocol negotiation and gets all of the static information set up
before returning the brain move making procedure.  

The macro that implements all of this is a simple |syntax-rules| macro. 
There are a few things we do have to make sure about. Firstly, we need 
to be careful not to use |name| in more than one place, since it could 
be an expression and not a regular identifier. Secondly, we want to 
make sure that we make as many things visible in the bindings of 
our |init| expression as we can, since this makes it possible to use 
some values like |walls| to populate the initial state. We do this 
in the above example brain that moves randomly. We also want to 
remember to wrap the body |b1 b2 ...| in a let-nil and not a 
|begin| so that the user can provide definitions at the top of the 
form if they want to.

@c () => (define-tron-brain)
@<Define |define-tron-brain|@>=
(define-syntax define-tron-brain
  (syntax-rules ()
    [(k (proc (name size walls play ppos opos) 
              (state init)) 
        b1 b2 ...)
     (define (proc play-port info-port)
       (format play-port "~a~n" name)
       (let ([play (make-play-proc 'play play-port)])
         (let* ([size (read info-port)] [walls (read info-port)])
           (let ([state init])
             (lambda (port)
               (let* ([ppos (read port)] [opos (read port)])
                 (let-values ([(newstate) (let () b1 b2 ...)])
                   (set! state newstate))))))))]))

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
  (let-values ([(b1-play-port b1-get) (open-string-output-port)]
               [(b2-play-port b2-get) (open-string-output-port)])
    (let ([ip1 (make-info-port size walls)]
          [ip2 (make-info-port size walls)])
      (let ([b1-play (b1 b1-play-port ip1)]
            [b2-play (b2 b2-play-port ip2)])
        (let ([name1 (parse-name (b1-get))] [name2 (parse-name (b2-get))])
          (printf "Name1: ~a~nName2: ~a~n" name1 name2)
          @<Simulate tron game@>)))))

@ Given the size of a board, and the walls, detect if it is
symmetrical across the diagonal.

@ Let's define some checkers for our game first.

@p
(define (valid-size? size)
  (and (pair? size)
       (integer? (car size))
       (integer? (cdr size))))
(define (valid-walls? walls)
  (and (list? walls)
       (for-all pair? walls)))
(define (valid-position? pos)
  (and (pair? pos)
       (integer? (car pos))
       (integer? (cdr pos))))

@ Because we are simulating the actual behavior of the server, we are 
using a number of different ports, where the real server might use 
only one for all of the communication. The first of these is our 
information port. This port should contain the board size and the 
wall information in the same format as the server will send it. In 
this case, this means just using |write| a few times.

@p
(define (make-info-port size walls)
  (open-string-input-port
    (format "~s~s~n" size walls)))

@ The tron game proceeds from the protocol above starting at step 4. 
We send out the the player and opponent positions and then we need 
to get the moves back from each player. 

@c (pos1 pos2 size walls b1-play b2-play b1-get b2-get name1 name2)
@<Simulate tron game@>=
(let loop ([pos1 pos1] [pos2 pos2] [walls walls])
  @<Print game position@>
  (let ([status (game-status pos1 pos2 walls)])
    (case status
      [(draw p1 p2) @<Print game result@>]
      [else
        (sleep (make-time 'time-duration 250000000 0))
        (let ([port1 (make-pos-port pos1 pos2)]
              [port2 (make-pos-port pos2 pos1)])
          (b1-play port1) (b2-play port2)
          (let ([m1 (string->move (b1-get))]
                [m2 (string->move (b2-get))])
            @<Get new positions and walls@>
            (loop new-pos1 new-pos2 new-walls)))])))

@ Position ports encode the first part of the fourth protocol stage, where 
we send the player positions to each player. In this case, we assume 
that the first argument is the first player to send, and the second argument 
the opponent and second position to send.

@p
(define (make-pos-port p1 p2)
  (open-string-input-port 
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

@c (pos1 pos2 size walls)
@<Print game position@>=
(let ol ([i 0])
  (unless (= i (cdr size))
    (let il ([j 0])
      (unless (= j (car size))
        (let ([p (cons j i)])
          (cond 
            [(member p walls)
             (if (or (equal? p pos1) (equal? p pos2))
                 (printf "⍣ ")
                 (printf "⌹ "))]
            [(equal? p pos1) (printf "⍋ ")]
            [(equal? p pos2) (printf "⍒ ")]
            [else (printf "∘ ")]))
        (il (1+ j))))
    (newline)
    (ol (1+ i))))
(newline)

@ Once we have the moves that the players have made, we want to make 
those positions and create the updated positions values. We also 
want to make sure that the walls of the current positions of the 
players are thrown into the walls set. 

@c (size pos1 pos2 m1 m2 walls) => (new-pos1 new-pos2 new-walls)
@<Get new positions and walls@>=
(define new-pos1 (get-pos m1 pos1 size))
(define new-pos2 (get-pos m2 pos2 size))
(define new-walls (cons* pos1 pos2 walls))

@ The |game-status| procedure takes the position of player1, position
of player2, and the current list of walls. If either player position
is the same as a wall, that player has crashed. If both players have
crashed, the game is a draw. If one player has crashed then the other
player has won. If neither player has crashed let the game continue!

@p
(define (game-status pos1 pos2 walls)
  (let ([p1-dead? (member pos1 walls)]
	[p2-dead? (member pos2 walls)])
    (cond
     [(and p1-dead? p2-dead?) 'draw]
     [p1-dead? 'p2]
     [p2-dead? 'p1]
     [else 'not-over])))

@ When the game has concluded, we want to print the 
results of the win. We will print out nice little 
messages for people here.
We also want to make sure that we send the correct 
states to each player.

@c (status name1 name2) 
@<Print game result@>=
(case status
  [(draw) (printf "The game was a draw.~n")]
  [(p1) (printf "~a (Player 1) has won!" name1)]
  [(p2) (printf "~a (Player 2) has won!" name2)]
  [else (error #f "invalid status" status)])

@* The Server Protocol. The general process of a client making 
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

\noindent {\it More information is needed here.}

@ Let's define a procedure |parse-name| that will handle the initial 
data that reads the name from the client. All we have to do is read 
the name from the first line of the client and strip the trailing and 
leading whitespace.

@p
(define (parse-name str)
  (define first-line
    (with-input-from-string str
      (lambda () (get-line (current-input-port)))))
  (define (strip lst)
    (or (memp (lambda (x) (not (char-whitespace? x))) lst)
        '()))
  (list->string
    (reverse (strip (reverse (strip (string->list first-line)))))))

@ We use the |make-play-proc| to create our procedures that will 
send the messages to the server. They create a |play| procedure 
that is closed over a particular port. In this case, since this 
is a forward or user facing procedure that we are returnning, 
we want to do some reasonable error checking here and report 
invalid moves that are sent to the server on the client side.

$$\.{make-play-proc} : \\{name}\times\\{port}\to(\\{move}\to\.{\#<void>})$$

\noindent 
We use the $\\{name}$ value when reporting errors, but it is otherwise
unnecessary for the actual computation.  We will also ensure that we
actually flush the buffer after we send our data, because we cannot be
sure that the port we are given will actually flush the data in the
way that we expect. At any rate, we make sure to flush and to put a
newline at the end of our line for completeness.

@p
(define (make-play-proc name port)
  (lambda (move)
    (unless (memq move valid-moves)
      (error name "invalid move" move))
    (format port "~s~n" move)
    (flush-output-port port)))

@* Running on a server.

@* Board Utilities. 

@ We provide the |get-pos| procedure, which takes a move or direction 
and an old position, and returns a new position assuming that we moved
in that new direction. 

$$\.{get-pos} : 
  \\{move}\times\\{position}\times\\{size}\to\\{new-position}$$

@p
(define (get-pos m old size)
  (let ([width (car size)] [height (cdr size)])
    (let ([x (car old)] [y (cdr old)])
      (case m 
        [(n) (cons x (mod (-1+ y) height))]
        [(w) (cons (mod (-1+ x) width) y)]
        [(e) (cons (mod (1+ x) width) y)]
        [(s) (cons x (mod (1+ y) height))]
        [else (error #f "invalid move" m)]))))

@ Make a classic tron game with an empty board

@p
(define (make-starting-walls size)
  (let ([x (car size)]
	[y (cdr size)])
    (append
     (map (lambda (v) `(,v . 0)) (iota x))
     (map (lambda (v) `(,v . ,(sub1 y))) (iota x))
     (map (lambda (v) `(0 . ,v)) (iota y))
     (map (lambda (v) `(,(sub1 x). ,v)) (iota y)))))

@* Quick Reference. The following is a set of quick references 
for those who are already familiar with the Tron API, but 
need a quick refresher of the major programming elements.

\bigskip
{\smallskip\noindent{\bf Defining brains.}
\smallskip\verbatim
(define-tron-brain (proc (name size walls play ppos opos) 
                         (state init))
  body+ ...)
!endverbatim\bigskip}

{\smallskip\noindent{\bf Playing a game.}
\smallskip\verbatim
(play-tron board-size board-walls 
  player1-position player2-position
  player1-brain    player2-brain)
!endverbatim\par}

@* Index.
