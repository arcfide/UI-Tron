
@* Introduction. This document details the implementation of an API 
for the tron game used in CSCI-B351. More information about the API 
for the users can be found on the Wiki site.

The tron api is focused on providing a simple but flexible interface 
for allowing pluggable game brains or A.I.'s to play a tron game. In 
tron, two players compete against one another for space. 

{\it Need some information about the game play.}

This library provides a basic API for testing brains against one 
another as well as for connecting to a game server to play the game.

@* Creating a tron brain. To create a tron brain, we provide a 
syntax that handles the creation of the boiler plate for you.

\medskip\verbatim
(define-tron-brain (name info get play) body+ ...)
!endverbatim\medskip

\noindent The above form binds |name| to a procedure that can be used 
to run a simulation. |name| will be bound to a procedure that expects 
to receive a single argument, which is a port, that allows it to 
communicate its moves to the driver, either locally or remotely. The
brain never needs to interact with that port though. All the brain has 
to do is to use the two |get| and |play| procedures that are bound 
to get the move of the opponent and to send its move to the engine. 
These two procedures have the following syntax:

$$\eqalign{\.{get} :\ \to \\{move}||\\{game-result}\cr
\.{play} : \\{move} \to \.{\#<void>}}$$

\noindent The |info| binding points to a record about the game information, 
including the size of the board and whether you are the first or second 
player. 

@p
(define-syntax define-tron-brain
  (syntax-rules ()
    [(_ (name info get play) b1 b2 ...)
     "Code must go here at some point XXX"]))

@* Simulating games.

@* Running on a server.

@* The tron server protocol.
