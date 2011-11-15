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
)(@< (Process\x20;named\x20;chunk sections loop tokens sectnum ) 
(unless (<= 3 (length tokens)) (error #f "unexpected end of file" tokens))
(let ([type (car tokens)] [name (cadr tokens)] [delim (caddr tokens)])
  (unless (string? name) (error #f "expected chunk name" name))
  (unless (memq delim '(@> @>=)) (error #f "invalid delimiter" delim))
  (hashtable-update! sections (strip-whitespace name)
    (lambda (cur) 
      (let ([defs (section-info-defs cur)]
            [refs (section-info-refs cur)])
        (when (and (section-info-type cur) 
                   (not (eq? type (section-info-type cur))))
          (error #f "section type mismatch" name))
        (case delim 
          [(@>) (make-section-info type defs (set-cons sectnum refs))]
          [(@>=) (make-section-info type (set-cons sectnum defs) refs)]
          [else (error #f "this can't happen")])))
    (make-section-info #f '() '()))
  (loop (list-tail tokens 3) sectnum))
)

(@< (Handle\x20;index\x20;token tokens index loop sectnum res ) 
(let ([code (car tokens)])
  Verify\x20;index\x20;syntax
  (index-db-insert index code (cadr tokens) sectnum)
  (loop (cdddr tokens) res sectnum))
)

(@< (Weave\x20;file\x20;chunk txttkns port sectnum encode sections ) 
(unless (<= 4 (length txttkns))
  (error #f "Missing pieces of a named chunk" txttkns))
(let ([name (list-ref txttkns 1)]
      [delim (list-ref txttkns 2)])
  (unless (string? name)
    (error #f "expected filename for chunk" name))
  (unless (eq? '@>= delim)
    (error #f "expected delimiter @>=" delim))
  (let-values ([(rest body) 
                (slurp-code (list-tail txttkns 3) encode texify-code)])
    (print-named-chunk
      port texify-filename name body sectnum '() #f sections)
    rest))
)

(@< (Parse\x20;possible\x20;control\x20;code\x20;and\x20;\x7C;loop\x7C; c cur tokens loop ports ) 
(let ([nc (read-char (car ports))])
  (case nc
    [(#\@) (loop tokens (cons c cur) ports)]
    [(#\q) (get-line (car ports)) (loop tokens cur ports)]
    [(#\space #\< #\p #\* #\e #\r #\( #\^ #\. #\: #\c) ;)
     Add\x20;buffer\x20;and\x20;control\x20;code\x20;to\x20;\x7C;tokens\x7C;\x20;and\x20;\x7C;loop\x7C;]
    [(#\>) Parse\x20;possible\x20;\x7C;@>=\x7C;\x20;delimiter\x20;and\x20;\x7C;loop\x7C;]
    [(#\i) Include\x20;new\x20;file\x20;in\x20;\x7C;ports\x7C;\x20;and\x20;\x7C;loop\x7C;]
    [else
      (if (eof-object? nc)
          (loop tokens cur ports)
          (loop tokens (cons* nc c cur) ports))]))
)

(@< (Weave\x20;optional\x20;code\x20;chunk tokens port sectnum encode sections ) 
(let ([txttkns (cddr tokens)])
  (cond
    [(null? txttkns) '()]
    [(not (symbol? (car txttkns)))
     (error #f "expected control code" (car txttkns))]
    [else
      (case (car txttkns)
        [(@p) Weave\x20;program\x20;chunk]
        [(@<)
         (let ([captures '()] [exports '()])
           Weave\x20;named\x20;chunk)]
        [(@c) Weave\x20;captures\x20;and\x20;named\x20;chunk]
        [(@|(|) Weave\x20;file\x20;chunk]
        [(|@ | @*) txttkns]
        [else
          (error #f "unrecognized code" (car txttkns))])]))
)

(@< (Dispatch\x20;on\x20;token\x20;type tokens index port res sectnum loop ) 
(cond
  [(null? tokens) Write\x20;index\x20;to\x20;file (reverse res)]
  [(memq (car tokens) '(@* |@ |))
   (loop (cdr tokens) (cons (car tokens) res) (1+ sectnum))]
  [(memq (car tokens) '(@^ @. @:)) Handle\x20;index\x20;token]
  [(eq? '@c (car tokens)) Index\x20;captures\x20;line]
  [(symbol? (car tokens))
   (loop (cdr tokens) (cons (car tokens) res) sectnum)]
  [(string? (car tokens)) Deal\x20;with\x20;string\x20;token]
  [else
    (error #f "unrecognized token" (car tokens))])
)

(@< (Weave\x20;sections port tokens next-section encode sections ) 
(let loop ([tokens tokens])
  (when (pair? tokens)
    (call-with-values 
      (lambda () Process\x20;a\x20;section)
      loop)))
)

(@< (Extend\x20;named\x20;chunk\x20;and\x20;\x7C;loop\x7C; loop tokens named current-captures current-exports captures ) 
Verify\x20;and\x20;extract\x20;delimited\x20;chunk
(let ([name (string->symbol (strip-whitespace name))])
  (hashtable-update! named name
    (lambda (cur) (string-append cur body))
    "")
  (hashtable-update! captures name
    (lambda (cur) Extend\x20;captures\x20;and\x20;exports)
    #f))
(loop tknsrest '() #f)
)

(@< (Weave\x20;captures\x20;and\x20;named\x20;chunk port sectnum txttkns encode sections ) 
(unless (and (pair? (cdr txttkns)) (string? (cadr txttkns)))
  (error #f "expected captures line"
    (list-head txttkns (min (length txttkns) 2))))
(let-values ([(captures exports) (parse-captures-line (cadr txttkns))])
  (let ([txttkns (cddr txttkns)])
    Weave\x20;named\x20;chunk))
)

(@< (Verify\x20;and\x20;extract\x20;delimited\x20;chunk tokens ) => (name body tknsrest )
(define-values (name body tknsrest)
  (let ()
    (unless (<= 4 (length tokens))
      (error #f "unexpected end of file" tokens))
    (let ([name (list-ref tokens 1)] [closer (list-ref tokens 2)]) 
      (unless (eq? '@>= closer)
        (error #f "Expected closing @>=" name closer)) 
      (unless (string? name)
        (error #f "Expected name string" name))
      (let-values ([(ntkns body) 
                    (slurp-code (list-tail tokens 3) 
                                tangle-encode 
                                (lambda (x) x))])
        (values name body ntkns)))))
)

(@< (Add\x20;buffer\x20;and\x20;control\x20;code\x20;to\x20;\x7C;tokens\x7C;\x20;and\x20;\x7C;loop\x7C; c nc cur tokens loop ports ) 
(let ([token (string->symbol (string c nc))])
  (if (null? cur)
      (loop (cons token tokens) '() ports)
      (loop (cons* token (list->string (reverse cur)) tokens)
            '() ports)))
)

(@< (Parse\x20;possible\x20;\x7C;@>=\x7C;\x20;delimiter\x20;and\x20;\x7C;loop\x7C; cur loop tokens c nc ports ) 
(define (extend tok ncur)
  (if (null? cur)
      (loop (cons tok tokens) ncur ports)
      (loop (cons* tok (list->string (reverse cur)) tokens)
            ncur ports)))
(let ([nnc (read-char (car ports))])
  (if (char=? #\= nnc)
      (begin (get-line (car ports)) (extend '@>= '()))
      (extend '@> (list nnc))))
)

(@< (Write\x20;tangled\x20;file\x20;contents file output-file top-level-chunks named-chunks captures ) 
(call-with-output-file output-file
  (lambda (output-port)
    (when (eq? file '*default*)
      (printf "Tangling ~a~n" output-file)
      (format output-port "#!chezscheme~n~a" runtime-code)
      Write\x20;named\x20;chunks\x20;to\x20;file)
    (unless (eq? file '*default*) (printf "Outputing ~a~n" output-file))
    (put-string output-port
      (hashtable-ref top-level-chunks 
        (if (eq? file '*default*) '*default* output-file) 
        "")))
  'replace)
)

(@< (Index\x20;captures\x20;line loop tokens sectnum res index ) 
(define (insert x) 
  (and (< 1 (string-length x))
       (index-db-insert index '@|\| x sectnum)))
(let ([body (cadr tokens)])
  (unless (string? body) (error #f "expected captures line" body))
  (let-values ([(captures exports) (parse-captures-line body)])
    (for-each insert (map symbol->string captures))
    (and exports (for-each insert (map symbol->string exports))))
  (loop (cddr tokens) (cons* (cadr tokens) (car tokens) res) sectnum))
)

(@< (Weave\x20;named\x20;chunk txttkns port sectnum captures exports encode sections ) 
(unless (<= 4 (length txttkns))
  (error #f "Missing pieces of a named chunk" txttkns))
(let ([name (list-ref txttkns 1)]
      [delim (list-ref txttkns 2)])
  (unless (string? name)
    (error #f "expected name for chunk" name))
  (unless (eq? '@>= delim)
    (error #f "expected delimiter @>=" delim))
  (let-values ([(rest body) 
                (slurp-code (list-tail txttkns 3) encode texify-code)])
    (print-named-chunk
      port texify-section-text name body sectnum captures exports
      sections)
    rest))
)

(@< (Write\x20;named\x20;chunks\x20;to\x20;file captures named-chunks output-port ) 
(let-values ([(keys vals) (hashtable-entries named-chunks)])
  (vector-for-each
    (lambda (name code)
      (let ([cell (hashtable-ref captures name '(() . #f))])
        (format output-port
          "(@< (~s ~{~s ~}) ~@[=> (~{~s ~})~]~n~a~n)~n~n"
          name (car cell) (cdr cell) code)))
    keys vals))
)

(@< (Deal\x20;with\x20;string\x20;token loop tokens res sectnum ) 
(loop (cdr tokens)
  (if (and (pair? res) (string? (car res)))
      (cons
        (string-append (car res) (car tokens))
        (cdr res))
      (cons (car tokens) res))
  sectnum)
)

(@< (Update\x20;the\x20;current\x20;captures\x20;and\x20;\x7C;loop\x7C; loop tokens ) 
(unless (string? (cadr tokens))
  (error #f "Expected captures line" (cadr tokens)))
(let-values ([(captures exports) (parse-captures-line (cadr tokens))])
  (loop (cddr tokens) captures exports))
)

(@< (Define\x20;section\x20;iterator ) => (next-section )
(define next-section
  (let ([section -1])
    (lambda ()
      (set! section (+ section 1))
      section)))
)

(@< (Process\x20;a\x20;starred\x20;section port tokens sectnum encode sections ) 
(define-values (depth body)
  Scrape\x20;depth\x20;and\x20;body\x20;from\x20;starred\x20;section)
(format port "\\N{~a}{~a}~a~n"
  depth sectnum (texify-section-text body))
(let ([leftover Weave\x20;optional\x20;code\x20;chunk])
  (format port "\\fi~n~n")
  leftover)
)

(@< (Process\x20;a\x20;section port tokens next-section encode sections ) 
(define sectnum (next-section))
(case (car tokens)
  [(|@ |) Process\x20;a\x20;normal\x20;section]
  [(@*) Process\x20;a\x20;starred\x20;section]
  [else
    (if (string? (car tokens))
        (begin (put-string port (car tokens))
               (cdr tokens))
        (error #f "Section start expected, but found something else."
          (list-head tokens (min (length tokens) 3))))])
)

(@< (Write\x20;index\x20;to\x20;file port index ) 
(define (print name macro sects)
  (format port "\\I~a{~a}, ~{~a~^, ~}.~n" macro name (list-sort < sects)))
(for-each
  (lambda (entry)
    (let ([name (car entry)]
          [ident (cdr (assq '@|\| (cdr entry)))]
          [roman (cdr (assq '@^ (cdr entry)))]
          [typew (cdr (assq '@. (cdr entry)))]
          [nine (cdr (assq '@: (cdr entry)))])
      (when (pair? ident) (print name "\\\\" ident))
      (when (pair? nine) (print name "\\9" nine))
      (when (pair? roman) (print name " " roman))
      (when (pair? typew) (print name "\\." typew))))
  (list-sort (lambda (a b) (string<=? (car a) (car b)))
    (let-values ([(key val) (hashtable-entries index)])
      (map cons (vector->list key) (vector->list val)))))
)

(@< (Extend\x20;captures\x20;and\x20;exports current-exports current-captures cur name ) 
(define (union s1 s2) 
  (fold-left (lambda (s e) (if (memq e s) s (cons e s))) s1 s2))
(when (and cur (not (cdr cur)) current-exports)
  (error #f
    "attempt to extend a value named chunk as a definition chunk"
    name current-exports))
(when (and cur (cdr cur) (not current-exports))
  (error #f "attempt to extend a definition chunk as a value chunk"
    name (cdr cur)))
(if cur
    (cons
      (append (car cur) current-captures)
      (and (cdr cur) (append (cdr cur) current-exports)))
    (cons current-captures current-exports))
)

(@< (Process\x20;a\x20;normal\x20;section port tokens sectnum encode sections ) 
(define body
  (let ([maybe (cadr tokens)])
    (unless (string? maybe)
      (error #f "Section contains no body." (list-head tokens 2)))
    maybe))
(format port "\\M{~a}~a~n" sectnum (texify-section-text body))
(let ([leftover Weave\x20;optional\x20;code\x20;chunk])
  (format port "\\fi~n~n")
  leftover)
)

(@< (Extend\x20;file\x20;top-level\x20;and\x20;\x7C;loop\x7C; loop tokens top-level ) 
Verify\x20;and\x20;extract\x20;delimited\x20;chunk
(let ([name (strip-whitespace name)])
  (hashtable-update! top-level name
    (lambda (cur) (string-append cur body))
    ""))
(loop tknsrest '() #f)
)

(@< (Extend\x20;default\x20;top-level\x20;and\x20;\x7C;loop\x7C; loop tokens top-level ) 
(define-values (ntkns body) 
  (slurp-code (cdr tokens) tangle-encode (lambda (x) x)))
(hashtable-update! top-level '*default*
  (lambda (cur) (string-append cur body))
  "")
(loop ntkns '() #f)
)

(@< (Scrape\x20;depth\x20;and\x20;body\x20;from\x20;starred\x20;section tokens ) 
(define orig
  (let ()
    (unless (string? (cadr tokens))
      (error #f "Section contains no body" (list-head tokens 2)))
    (cadr tokens)))
(define (strip-whitespace lst)
  (cond
    [(null? lst) '()]
    [(char-whitespace? (car lst)) (strip-whitespace (cdr lst))]
    [else lst]))
(define (extract-number cur body)
  (cond
    [(null? body) (error #f "Section contains no body" orig)]
    [(char-numeric? (car body))
     (extract-number (cons (car body) cur) (cdr body))]
    [else
      (if (null? cur)
          (values 1 (list->string body))
          (values
            (string->number (list->string (reverse cur)))
            (list->string body)))]))
(let ([body (strip-whitespace (string->list orig))])
  (cond
    [(null? body) (error #f "Section contains no body" orig)]
    [(char=? #\* (car body)) (values 0 (list->string (cdr body)))]
    [else (extract-number '() body)]))
)

(@< (Dispatch\x20;on\x20;control\x20;code loop tokens top-level current-captures current-exports named captures ) 
(case (car tokens)
  [(|@ | @* @e @r @^ @. @: @i) (loop (cddr tokens) '() #f)]
  [(@p) Extend\x20;default\x20;top-level\x20;and\x20;\x7C;loop\x7C;]
  [(@<) Extend\x20;named\x20;chunk\x20;and\x20;\x7C;loop\x7C;]
  [(|@(|) Extend\x20;file\x20;top-level\x20;and\x20;\x7C;loop\x7C;]
  [(@c) Update\x20;the\x20;current\x20;captures\x20;and\x20;\x7C;loop\x7C;]
  [else (error #f "Unexpected token" (car tokens) (cadr tokens))])
)

(@< (Finish\x20;tokenizing\x20;and\x20;return\x20;token\x20;list tokens cur ) 
(reverse
  (if (null? cur)
      tokens
      (cons (list->string (reverse cur)) tokens)))
)

(@< (Include\x20;new\x20;file\x20;in\x20;\x7C;ports\x7C;\x20;and\x20;\x7C;loop\x7C; loop ports cur tokens ) 
(let ([fname (with-input-from-string (get-line (car ports)) read)])
  (unless (string? fname)
    (error #f "expected string file name" fname))
  (loop (if (pair? cur)
            (cons (list->string (reverse cur)) tokens)
            tokens)
        '()
        (cons fname ports)))
)

(@< (Define\x20;weave\x20;chunk\x20;reference\x20;encoder sections ) => (encode )
(define (encode x)
  (let ([res (hashtable-ref sections x (make-section-info '@< '() '()))])
    (format "!X~a:~?!X"
      (let ([defs (section-info-defs res)])
        (if (null? defs) "" (car defs)))
      (let ([type (section-info-type res)])
        (case type [(@<) "!rm ~a!tt"] [(@|(|) "\\\\{~a}"])) ; )
      (list x))))
)

(@< (Verify\x20;chunk\x20;reference\x20;syntax tokens ) 
(unless (<= 3 (length tokens))
  (error #f "unexpected end of token stream" tokens))
(unless (string? (cadr tokens))
  (error #f "expected chunk name" (list-head tokens 2)))
(unless (eq? '@> (caddr tokens))
  (error #f "expected chunk closer" (list-head tokens 3)))
)

(@< (Define\x20;slurp\x20;verifier tokens ) => (verify )
(define (verify x)
  (when (zero? (string-length x))
    (error #f "expected code body" 
      (list-head tokens (min (length tokens) 3))))
  (when (for-all char-whitespace? (string->list x))
    (error #f "empty chunk body" x))
  (strip-whitespace x))
)

(@< (Weave\x20;program\x20;chunk port txttkns sectnum encode ) 
(let-values ([(rest body) (slurp-code (cdr txttkns) encode texify-code)])
  (format port "\\Y\\B ~a \\par~n" (chezweb-pretty-print body))
  rest)
)

(@< (Construct\x20;chunk\x20;tables\x20;\x7C;named\x7C;\x2C;\x20;\x7C;top-level\x7C;\x2C;\x20;and\x20;\x7C;captures\x7C; tokens ) 
(let ([named (make-eq-hashtable)]
      [top-level (make-hashtable equal-hash equal?)]
      [captures (make-eq-hashtable)])
  (let loop ([tokens (if (string? (car tokens)) (cdr tokens) tokens)] 
             [current-captures '()]
             [current-exports #f])
    (if (null? tokens)
        (values top-level named captures)
        Dispatch\x20;on\x20;control\x20;code)))
)

(@< (Finish\x20;tokenizing\x20;port\x20;and\x20;\x7C;loop\x7C; loop tokens cur ports ) 
(if (null? cur)
    (loop tokens cur (cdr ports))
    (loop (cons (list->string (reverse cur)) tokens)
          '()
          (cdr ports)))
)

(@< (Write\x20;sections\x20;index sections file sections ) 
(define (print-index port nums name type)
  (format port "\\I\\X~{~a~^, ~}:~?\\X~n~@[~a~n~]"
    (list-sort < nums)
    (case type [(@|(|) "\\\\{~a}"] [(@<) "~a"]) ;)
    (list name)
    (weave-sec-refs sections name)))(call-with-output-file (format "~a.scn" (path-root file))
  (lambda (port)
    (for-each 
      (lambda (e)
        (let ([name (car e)]
              [nums (section-info-defs (cdr e))]
              [type (section-info-type (cdr e))])
          (print-index port nums name type)))
      (let-values ([(keys vals) (hashtable-entries sections)])
        (list-sort (lambda (a b) (string<? (car a) (car b)))
          (map cons (vector->list keys) (vector->list vals))))))
  'replace)
)

(@< (Verify\x20;index\x20;syntax tokens ) 
(unless (<= 3 (length tokens))
  (error #f "invalid index entry" tokens))
(unless (string? (cadr tokens))
  (error #f "expected index entry text" (list-head tokens 3)))
(unless (eq? '@> (caddr tokens))
  (error #f "expected index entry closer" (list-head tokens 3)))
)

(define (display-chezweb-version tangle/weave)
  (printf "This is ~a, ChezWEB Version 2.0 Beta.~n" tangle/weave))(define (chezweb-tokenize port)
  (let loop ([tokens '()] [cur '()] [ports (list port)])
    (if (null? ports)
        Finish\x20;tokenizing\x20;and\x20;return\x20;token\x20;list
        (let ([c (read-char (car ports))])
          (cond
           [(eof-object? c) Finish\x20;tokenizing\x20;port\x20;and\x20;\x7C;loop\x7C;]
           [(char=? #\@ c) Parse\x20;possible\x20;control\x20;code\x20;and\x20;\x7C;loop\x7C;]
           [else (loop tokens (cons c cur) ports)])))))(define (slurp-code tokens encode clean)
  Define\x20;slurp\x20;verifier
  (let loop ([tokens tokens] [res ""])
    (cond
      [(null? tokens) (values '() (verify res))]
      [(string? (car tokens))
       (loop (cdr tokens) 
             (string-append res (clean (car tokens))))]
      [(eq? '@< (car tokens))
       Verify\x20;chunk\x20;reference\x20;syntax
       (loop (cdddr tokens)
         (string-append
           res (encode (strip-whitespace (cadr tokens)))))]
      [else (values tokens (verify res))])))(define-syntax (get-code x)
  (call-with-input-file "runtime.ss" get-string-all))
(define runtime-code (get-code))(define (tangle-encode x) (format "~s" (string->symbol x)))(define (parse-captures-line str)
  (with-input-from-string str
    (lambda ()
      (let* ([captures (read)] [arrow (read)] [exports (read)])
        (unless (and (list? captures) (for-all symbol? captures))
          (error #f
            "Expected list of identifiers for captures" captures))
        (unless (and (eof-object? arrow) (eof-object? exports))
          (unless (eq? '=> arrow)
            (error #f "Expected =>" arrow))
          (unless (and (list? exports) (for-all symbol? exports))
            (error #f
              "Expected list of identifiers for exports" exports)))
        (values captures (and (not (eof-object? exports)) exports))))))(define (strip-whitespace str)
  (define (search str inc start end)
    (let loop ([i start])
      (cond
        [(= i end) #f]
        [(not (char-whitespace? (string-ref str i))) i]
        [else (loop (inc i))])))
  (let ([s (search str 1+ 0 (string-length str))]
        [e (search str -1+ (-1+ (string-length str)) -1)])
    (or (and (not s) (not e) "")
        (substring str s (1+ e)))))(define (tangle-file web-file)
  (let ([default-file (format "~a.ss" (path-root web-file))]
        [tokens
          (cleanse-tokens-for-tangle
            (call-with-input-file web-file chezweb-tokenize))])
    (let-values ([(top-level-chunks named-chunks captures)
                  Construct\x20;chunk\x20;tables\x20;\x7C;named\x7C;\x2C;\x20;\x7C;top-level\x7C;\x2C;\x20;and\x20;\x7C;captures\x7C;])
      (for-each
        (lambda (file)
          (let ([output-file (if (eq? '*default* file)
                                 default-file file)])
            Write\x20;tangled\x20;file\x20;contents))
        (vector->list (hashtable-keys top-level-chunks))))))(define (weave-file file)
  (call-with-output-file (format "~a.tex" (path-root file))
    (lambda (port)
      Define\x20;section\x20;iterator
      (define tokens
        (write-index file (call-with-input-file file chezweb-tokenize)))
      (define sections (index-sections file tokens))
      Define\x20;weave\x20;chunk\x20;reference\x20;encoder
      (printf "Weaving ~a~n" file)
      (format port "\\input chezwebmac~n~n")
      Weave\x20;sections
      (format port "\\inx~n\\fin~n\\con~n"))
    'replace))(define (print-named-chunk port texify name code sectnum caps exps sections)
  (format port
    "\\Y\\B\\4\\X~a:~a\\X${}~@[~a~]\\E{}$\\6~n~a\\par~n~?~?~@[~?~]"
    sectnum (texify name)
    (and (not (weave-sec-def? sections name sectnum)) "\\mathrel+")
    (chezweb-pretty-print code)
    "~@[\\CAP ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}.~]"
    (list (and (not (null? caps)) caps))
    "~@[\\EXP ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}.~]"
    (list exps)
    (and (weave-sec-def? sections name sectnum) "~@[~a~]~@[~a~]")
    (list (weave-sec-defs sections name) (weave-sec-refs sections name))))(define (texify-code code)
  (let loop ([code (string->list code)] [res '()])
    (cond
      [(null? code) (list->string (reverse res))]
      [(char=? #\! (car code)) (loop (cdr code) (cons* #\! #\! res))]
      [else (loop (cdr code) (cons (car code) res))])))
(define (chezweb-pretty-print code)
  (with-output-to-string
    (lambda ()
      (printf "\\verbatim~n")
      (printf "~a" code)
      (printf "!endverbatim "))))(define (texify-section-text text)
  (let loop ([text (string->list text)] [res '()] [bar? #f])
    (cond
      [(null? text) (list->string (reverse res))]
      [(char=? #\| (car text))
       (loop (cdr text) (cons #\| res) (not bar?))]
      [(char=? #\! (car text))
       (if bar?
           (loop (cdr text) (cons* #\! #\! res) bar?)
           (loop (cdr text) (cons #\! res) bar?))]
      [else
        (loop (cdr text) (cons (car text) res) bar?)])))(define (texify-filename txt)
  (format "\\\\{~a}" txt))(define (write-index file tokens)
  (let ([ofile (format "~a.idx" (path-root file))]
        [index (make-hashtable string-hash string=?)])
    (printf "Writing index file...~n")
    (call-with-output-file ofile
      (lambda (port)
        (let loop ([tokens tokens] [res '()] [sectnum 0])
          Dispatch\x20;on\x20;token\x20;type))
      'replace)))(define (set-cons x lst) (if (memv x lst) lst (cons x lst)))(define (index-db-insert index code entry sectnum)
  (hashtable-update! index (strip-whitespace entry)
    (lambda (db)
      (let ([res (assq code db)])
        (set-cdr! res (set-cons sectnum (cdr res)))
        db))
    (map list '(@^ @. @: @|\|))))(define (cleanse-tokens-for-tangle tokens)
  (let loop ([tokens tokens] [res '()])
    (cond
      [(null? tokens) (reverse res)]
      [(memq (car tokens) '(@: @^ @.))
       Verify\x20;index\x20;syntax
       (loop (cdddr tokens) res)]
      [(string? (car tokens))
       (if (and (pair? res) (string? (car res)))
           (loop (cdr tokens)
             (cons (string-append (car res) (car tokens)) (cdr res)))
           (loop (cdr tokens) (cons (car tokens) res)))]
      [else
        (loop (cdr tokens) (cons (car tokens) res))])))(define (index-sections file tokens)
  (printf "Writing section index...~n")
  (let ([sections (make-hashtable string-hash string=?)])
    (let loop ([tokens tokens] [sectnum 0])
      (when (pair? tokens)
        (case (car tokens)
          [(|@ | @*) (loop (cdr tokens) (1+ sectnum))]
          [(@< |@(|) Process\x20;named\x20;chunk] ;)
          [else (loop (cdr tokens) sectnum)])))
    Write\x20;sections\x20;index
    sections))(define-record-type section-info 
  (fields type defs refs)
  (protocol
    (lambda (n)
      (lambda (type defs refs)
        (unless (or (not type) (memq type '(@< @|(|))) ; )
          (error #f "invalid type" type))
        (unless (and (list? defs) (for-all integer? defs))
          (error #f "invalid defs list" defs))
        (unless (and (list? refs) (for-all integer? refs))
          (error #f "invalid refs list" refs))
        (n type defs refs)))))(define (weave-sec-defs sections name)
  (let ([res (hashtable-ref sections (strip-whitespace name) #f)])
    (and res 
         (let ([defs (section-info-defs res)])
           (and (pair? defs) (>= (length defs) 2)
                (format "\\A~[~;~{~s~}~;s~{~s and ~s~}~
                         ~:;s~{~#[~; and ~] ~s~^,~}~]."
                        (length defs) (list-sort < defs)))))))(define (weave-sec-refs sections name)
  (let ([res (hashtable-ref sections (strip-whitespace name) #f)])
    (and res 
         (let ([refs (section-info-refs res)])
           (and (pair? refs) 
                (format "\\U~[~;~{~s~}~;s~{~s and ~s~}~
                         ~:;s~{~#[~; and ~] ~s~^,~}~]."
                        (length refs) (list-sort < refs)))))))(define (weave-sec-def? sections name num)
  (let ([res (hashtable-ref sections (strip-whitespace name) #f)])
    (and res 
         (let ([defs (section-info-defs res)])
           (and (pair? defs) (= num (car (list-sort < defs))))))))