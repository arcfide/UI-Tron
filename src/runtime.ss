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
)