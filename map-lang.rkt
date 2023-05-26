#lang racket/base

(require (for-syntax racket/base)
         syntax/parse/define)

(provide require
         (rename-out [mb #%module-begin]))

(begin-for-syntax
  (define-splicing-syntax-class tile-def
    #:attributes [token tile-name]
    #:datum-literals [=]
    [pattern {~seq token:id = tile-name:id}])
  )

(define-syntax-parser mb
  #:literals (require)
  [(_ {~and req-decl (require reqd ...)}
      tdef:tile-def ...)
   #'(#%module-begin req-decl)])