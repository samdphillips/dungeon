#lang racket/base

(require racket/class
         racket/dict
         racket/draw
         racket/gui
         pict
         threading)

(provide make-tile-viewer)


(module+ main
  (define-syntax-rule (define-tile name)
    (define name (dynamic-require "tiles.rkt" 'name)))

  (define-syntax-rule (define-tile* names ...)
    (begin (define-tile names) ...))

  (define-tile*
    wall-banner-red
    wall-banner-blue
    wall-banner-green
    wall-banner-yellow
    lizard-m-idle-anim-0
    lizard-m-idle-anim-1
    lizard-m-idle-anim-2
    lizard-m-idle-anim-3)
  
  (define sprites
    (list
     (cons "wall-banner-red" (list wall-banner-red))
     (cons "wall-banner-blue" (list wall-banner-blue))
     (cons "wall-banner-green" (list wall-banner-green))
     (cons "wall-banner-yellow" (list wall-banner-yellow))
     (cons "lizard-m-idle-anim"
           (list lizard-m-idle-anim-0
                 lizard-m-idle-anim-1
                 lizard-m-idle-anim-2
                 lizard-m-idle-anim-3))))

  (make-tile-viewer sprites 32 32))

(define SCALE 8)

(define (atlas-selected-pict sprites selected)
  (define selected-pict #f)
  
  (define (selected-highlight p)
    (set! selected-pict
          (cc-superimpose
           (filled-rounded-rectangle
            (pict-width p) (pict-height p)
            #:color "skyblue")
           p))
    selected-pict)
  
  (define (label-sprite sprite-def)
    (match-define (list label sprite _ ...) sprite-def)
    (define wrap
      (if (string=? label selected)
          selected-highlight            
          values))
    (wrap (inset (vc-append sprite (text label)) 5)))

  (define (pad-list size xs)
    (define need (- size (modulo (length xs) size)))
    (append xs (make-list need (blank 0 0))))
  
  (define ncols 4 #;
    (~> (length sprites)
        sqrt
        ceiling
        inexact->exact))

  (values
   (table ncols
          (pad-list ncols (map label-sprite sprites))
          cc-superimpose
          cb-superimpose
          5
          2)
   selected-pict))


(define (make-tile-viewer sprites min-width min-height)
  (define current-sprite-name (caar sprites))
  (define current-frame 0)

  (define atlas-pict #f)

  (define ((update-frame x) . _rest)
    (set! current-frame (+ current-frame x))
    (update!))

  (define (current-frame-count)
    (length (dict-ref sprites current-sprite-name)))

  (define (current-frame-message-string)
    (~a (add1 current-frame) "/" (current-frame-count)))

  (define (change-sprite selector event)
    (unless (string=? current-sprite-name
                      (send selector get-string-selection))
      (set! current-frame 0)
      (set! current-sprite-name
            (send selector get-string-selection))
      (update!)))    

  (define (paint canvas dc)
    (define-values (x-scale y-scale) (send dc get-scale))
    (send dc set-scale SCALE SCALE)
    (define current-sprite
      (~>> (send selector get-string-selection)
           (dict-ref sprites)
           (list-ref _ current-frame)))
    (draw-pict current-sprite dc 0 0)
    (send dc set-scale x-scale y-scale))

  (define (paint-atlas-canvas canvas dc)        
    (draw-pict atlas-pict dc 0 0))
    
  
  (define (update!)  
    (send prev-button enable (not (zero? current-frame)))
    (send next-button enable
          (< (add1 current-frame) (current-frame-count)))
    (send frame-message set-label
          (current-frame-message-string))
    
    (define selected
      (send selector get-string-selection))
    (define-values (atlas-pict-new selected-pict)
      (atlas-selected-pict sprites selected))
    (set! atlas-pict atlas-pict-new)
    (define-values (scroll-x scroll-y)
      (cc-find atlas-pict selected-pict))
    (define-values (scroll-w scroll-h)
      (send atlas-canvas get-virtual-size))
    (send atlas-canvas scroll
          (/ scroll-x scroll-w) (/ scroll-y scroll-h))
    
    (send sprite-canvas refresh)
    (send atlas-canvas refresh))

  (define frame
    (new frame% [label "tile-viewer"]))

  (define vp
    (new vertical-pane%
         [parent frame]))

  (define hp1
    (new horizontal-pane%
         [stretchable-height #f]
         [parent vp]))

  (define hp2
    (new horizontal-pane%         
         [parent vp]))
  
  (define selector
    (new choice%
         [label "View"]
         [parent hp1]
         [callback change-sprite]
         [choices (map car sprites)]))

  (define prev-button
    (new button%
         [label "<"]
         [parent hp1]
         [callback (update-frame -1)]))

  (define frame-message
    (new message%
         [label (current-frame-message-string)]
         [parent hp1]))

  (define next-button
    (new button%
         [label ">"]
         [parent hp1]
         [callback (update-frame 1)]))

  (define sprite-canvas
    (new canvas%
         [parent hp2]
         [paint-callback paint]
         [min-width (* SCALE min-width)]
         [min-height (* SCALE min-height)]))

  (define atlas-canvas
    (new canvas%
         [parent hp2]
         [paint-callback paint-atlas-canvas]
         [style '(hscroll vscroll)]
         [min-width (* SCALE min-width)]
         [min-height (* SCALE min-height)]))

  (let-values ([(p h) (atlas-selected-pict sprites "")])
    (let ([w (inexact->exact (ceiling (pict-width p)))]
          [h (inexact->exact (ceiling (pict-height p)))])
      (send atlas-canvas init-auto-scrollbars w h 0 0)))    
  
  (send frame show #t)
  (update!)
  frame)