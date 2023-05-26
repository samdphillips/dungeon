#lang racket/base

(require racket/class
         racket/draw         
         pict)

(provide sprite)

(define (sprite atlas-bitmap src-x src-y w h)
  (dc (lambda (dc dest-x dest-y)
        (define smooth (send dc get-smoothing))
        (send dc set-smoothing 'unsmoothed)
        (send dc draw-bitmap-section
              atlas-bitmap
              dest-x dest-y
              src-x src-y
              w h)
        (send dc set-smoothing smooth))
      w h))

#|
(define-runtime-path DUNGEON-TILESET "0x72_DungeonTilesetII_v1.3.png")

(define bmap
  (read-bitmap DUNGEON-TILESET))

bmap

(define floor-1 (sprite bmap 16 64 16 16))
(define floor-ladder (sprite bmap 48 96 16 16))

(scale (vl-append
        (ht-append floor-1 floor-1 floor-1 floor-1 floor-1)
        (ht-append floor-1 floor-1 floor-1 floor-1 floor-1)
        (ht-append floor-1 floor-1 floor-ladder floor-1 floor-1)
        (ht-append floor-1 floor-1 floor-1 floor-1 floor-1)
        (ht-append floor-1 floor-1 floor-1 floor-1 floor-1))
       2)
|#