#!chezscheme
(module (@< =>)
  (import-only (chezscheme))

(define-syntax @<
  (syntax-rules (=>)
    [(_ (name c ...) => (e ...) b1 b2 ...)
     (for-all identifier? #'(name c ... e ...))
     (module-form name (c ...) (e ...) b1 b2 ...)]
    [(_ (name c ...) b1 b2 ...)
     (value-form name (c ...) b1 b2 ...)]))(define-syntax (build-value-form x)
  (syntax-case x ()
    [(_ id (ic ...) body ...)
     (with-syntax ([(oc ...) (datum->syntax #'id (syntax->datum #'(ic ...)))])
       #'(let () (alias ic oc) ... body ...))]))(define-syntax value-form
  (syntax-rules ()
    [(_ name (c ...) body ...)
     (define-syntax (name x)
       (syntax-case x ()
         [id (identifier? #'id)
          #'(build-value-form id (c ...) ((... ...) body) ...)]
         [(id . rest)
          #'((build-value-form id (c ...) ((... ...) body) ...) 
             . rest)]))]))(define-syntax (build-module-form x)
  (syntax-case x ()
    [(_ id (ic ...) (ie ...) body ...)
     (with-syntax ([(oc ...) (datum->syntax #'id (syntax->datum #'(ic ...)))]
                   [(oe ...) (datum->syntax #'id (syntax->datum #'(ie ...)))])
       #'(module (oe ...)
           (alias ic oc) ...
           (module (ie ...) body ...)
           (alias oe ie) ...))]))(define-syntax module-form
  (syntax-rules ()
    [(_ name (c ...) (e ...) body ...)
     (define-syntax (name x)
       (syntax-case x ()
         [id (identifier? #'id)
          #'(build-module-form id (c ...) (e ...)
              ((... ...) body) ...)]))]))(indirect-export @<
  module-form value-form build-module-form build-value-form)
)(@< (Print\x20;game\x20;result status name1 name2 ) 
(case status
  [(draw) (printf "The game was a draw.~n")]
  [(p1) (printf "~a (Player 1) has won!" name1)]
  [(p2) (printf "~a (Player 2) has won!" name2)]
  [else (error #f "invalid status" status)])
)

(@< (Simulate\x20;tron\x20;game pos1 pos2 size walls b1-play b2-play b1-get b2-get name1 name2 ) 
(let loop ([pos1 pos1] [pos2 pos2] [walls walls])
  (print-board size walls pos1 pos2)
  (let ([status (game-status pos1 pos2 walls)])
    (case status
      [(draw p1 p2) Print\x20;game\x20;result]
      [else
        (sleep (make-time 'time-duration 250000000 0))
        (let ([port1 (make-pos-port pos1 pos2)] 
              [port2 (make-pos-port pos2 pos1)])
          (b1-play port1) (b2-play port2)
          (loop (get-pos (string->move (b1-get)) pos1 size)
                (get-pos (string->move (b2-get)) pos2 size)
                (cons* pos1 pos2 walls)))])))
)

(@< (Verify\x20;\x7C;play-tron\x7C;\x20;arguments size walls pos1 pos2 b1 b2 ) 
(unless (valid-size? size)
  (error 'play-tron "invalid board size" size))
(unless (valid-walls? size walls)
  (error 'play-tron "invalid walls list" walls))
(unless (valid-position? size pos1)
  (error 'play-tron "invalid cycle position" pos1))
(unless (valid-position? size pos2)
  (error 'play-tron "invalid cycle position" pos2))
(unless (procedure? b1)
  (error 'play-tron "invalid brain" b1))
(unless (procedure? b2)
  (error 'play-tron "invalid brain" b2))
)

(@< (Define\x20;\x7C;define-tron-brain\x7C; ) => (define-tron-brain )
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
)

(@< (Print\x20;representation\x20;for\x20;position walls i j pos1 pos2 ) 
(define p (cons j i))
(cond
  [(member p walls)
   (if (or (equal? p pos1) (equal? p pos2))
       (printf "⍣ ")
       (printf "⌹ "))]
  [(equal? p pos1) (printf "⍋ ")]
  [(equal? p pos2) (printf "⍒ ")]
  [else (printf "∘ ")])
)

(define valid-moves '(n w s e))Define\x20;\x7C;define-tron-brain\x7C;

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
      new-walls)))(define (play-tron size walls pos1 pos2 b1 b2)
  Verify\x20;\x7C;play-tron\x7C;\x20;arguments
  (let-values ([(b1-play-port b1-get) (open-string-output-port)]
               [(b2-play-port b2-get) (open-string-output-port)])
    (let ([ip1 (make-info-port size walls)]
          [ip2 (make-info-port size walls)])
      (let ([b1-play (b1 b1-play-port ip1)]
            [b2-play (b2 b2-play-port ip2)])
        (let ([name1 (parse-name (b1-get))] [name2 (parse-name (b2-get))])
          (printf "Name1: ~a~nName2: ~a~n" name1 name2)
          Simulate\x20;tron\x20;game)))))(define (game-status pos1 pos2 walls)
  (let ([p1-dead? (member pos1 walls)]
        [p2-dead? (member pos2 walls)])
    (cond
      [(and p1-dead? p2-dead?) 'draw]
      [p1-dead? 'p2]
      [p2-dead? 'p1]
      [else 'not-over])))(define (valid-size? size)
  (and (pair? size)
       (integer? (car size)) (integer? (cdr size))
       (positive? (car size)) (positive? (cdr size))))

(define (valid-walls? size walls)
  (and (list? walls)
       (for-all (lambda (pos) (valid-position? size pos)) walls)))

(define (valid-position? size pos)
  (and (pair? pos)
       (integer? (car pos)) (integer? (cdr pos))
       (<= 0 (car pos)) (<= 0 (cdr pos))
       (< (car pos) (car size)) (< (cdr pos) (cdr size))))(define (parse-name str)
  (define first-line
    (with-input-from-string str
      (lambda () (get-line (current-input-port)))))
  (define (strip lst)
    (or (memp (lambda (x) (not (char-whitespace? x))) lst)
        '()))
  (list->string
    (reverse (strip (reverse (strip (string->list first-line)))))))(define (make-play-proc name port)
  (lambda (move)
    (unless (memq move valid-moves)
      (error name "invalid move" move))
    (format port "~s~n" move)
    (flush-output-port port)))(define (make-info-port size walls)
  (open-string-input-port
    (format "~s~s~n" size walls)))(define (make-pos-port p1 p2)
  (open-string-input-port 
    (format "~s~s~n" p1 p2)))(define (string->move str)
  (let ([val (with-input-from-string str read)])
    (or (and (symbol? val) (member val valid-moves) val)
        (error #f "Not a valid move" val))))(define (play-remote-tron host port brain)
  (error 'play-remote-tron "remote play not implemented"))(define (print-board size walls pos1 pos2)
  (let ol ([i 0])
    (unless (= i (cdr size))
      (let il ([j 0])
        (unless (= j (car size))
          Print\x20;representation\x20;for\x20;position
          (il (1+ j))))
      (newline)
      (ol (1+ i))))
  (newline))(define (get-pos m old size)
  (let ([width (car size)] [height (cdr size)])
    (let ([x (car old)] [y (cdr old)])
      (case m 
        [(n) (cons x (mod (-1+ y) height))]
        [(w) (cons (mod (-1+ x) width) y)]
        [(e) (cons (mod (1+ x) width) y)]
        [(s) (cons x (mod (1+ y) height))]
        [else (error #f "invalid move" m)]))))(define (make-outer-walls size)
  (let ([x (car size)]
        [y (cdr size)])
    (append
      (map (lambda (v) `(,v . 0)) (iota x))
      (map (lambda (v) `(,v . ,(sub1 y))) (iota x))
      (map (lambda (v) `(0 . ,v)) (iota y))
      (map (lambda (v) `(,(sub1 x). ,v)) (iota y)))))(define (fair-game? size walls pos1 pos2)
  (error 'fair-game? "not implemented yet"))