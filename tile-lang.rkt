#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/draw
         racket/runtime-path         
         syntax/parse
         syntax/parse/define
         pict
         "sprite.rkt"
         "tile-viewer.rkt")

(provide #%top-interaction
         #%app
         #%datum
         (all-from-out pict)
         (rename-out
          [mb #%module-begin]))

(begin-for-syntax
  (define-syntax-class int
    [pattern i:exact-nonnegative-integer])
  
  (define-splicing-syntax-class tile-spec
    #:attributes (definer name args)
    (pattern {~seq name:id a:int b:int c:int d:int}
             #:attr args #'(a b c d)
             #:attr definer #'define-sprite)
    (pattern {~seq name:id a:int b:int c:int d:int e:int}
             #:attr args #'(a b c d e)
             #:attr definer #'define-sprite*))

  (define (add-declared-sprite! name-stx frame-stx)
    (define name-string
      (datum->syntax
       name-stx (symbol->string (syntax->datum name-stx))))
    (define b (syntax-local-value #'declared-sprites))
    (set-box! b (cons (cons name-string frame-stx) (unbox b))))
  )

(define-syntax declared-sprites (box null))

(define-syntax all-declared-sprites
  (lambda (stx)
    (define sprites
      (reverse
       (unbox (syntax-local-value #'declared-sprites))))
    #`(list
       #,@(for/list ([sprite (in-list sprites)])
            (define name-string (car sprite))
            (define ids (cdr sprite))
            #`(cons #,name-string (list #,@ids))))))           

(define-syntax-parser mb
  [(_ atlas-path:string
      {~optional {~seq #:width width:int #:height height:int}
                 #:defaults ([width #'16] [height #'16])}
      t:tile-spec ...)
   #:with all-sprites (datum->syntax #'atlas-path 'all-sprites)
   #'(#%module-begin
      (define-runtime-path ATLAS-PATH 'atlas-path)
      (define atlas-bitmap (read-bitmap ATLAS-PATH))
      (t.definer t.name #f atlas-bitmap . t.args) ...
      (define all-sprites (all-declared-sprites))
      (module+ main (make-tile-viewer all-sprites width height)))])

(define-syntax-parser define-sprite
  [(_ name anim? atlas-bitmap x:int y:int w:int h:int)
   (unless (syntax->datum #'anim?)
     (add-declared-sprite! #'name #'(name)))
   #'(begin
       (define name (sprite atlas-bitmap x y w h))
       (provide name))])

(define-syntax-parser define-sprite*
  [(_ name-base _ atlas-bitmap
      x-base:int y:int width:int height:int count:int)
   #:do [(define num-frames (syntax->datum #'count))
         (define x0 (syntax->datum #'x-base))
         (define w (syntax->datum #'width))]
   #:with (names ...)
   (for/list ([i (in-range num-frames)])
     (format-id #'name-base "~a-~a" #'name-base i))
   #:with (x ...)
   (for/list ([i (in-range num-frames)]
              [x (in-range x0 +inf.0 w)])
     (datum->syntax #'x-base x))
   (add-declared-sprite! #'name-base #'(names ...))
   #'(begin
       (define-sprite names #t atlas-bitmap x y width height) ...)])

