#lang racket/base
(require racket/class
         wxme
         "private/image-core-snipclass.rkt"
         "image-core.rkt")
(provide reader image<%>)

(define reader
  (new 
   (class* object% (snip-reader<%>)
     (define/public (read-header vers stream) (void))
     (define/public (read-snip text? cvers stream)
       (define bytes (send stream read-raw-bytes '2htdp/image))
       (define-values (new-bts separately-written-bytes-ht)
         (cond
           [(equal? bytes #"bmps-then-parsed")
            (define bytes-count (send stream read-integer '2htdp/image))
            (define separately-written-bytes-ht (make-hash))
            (for ([i (in-range bytes-count)])
              (hash-set! separately-written-bytes-ht i (send stream read-raw-bytes '2htdp/image)))
            (values (send stream read-raw-bytes '2htdp/image) separately-written-bytes-ht)]
           [else
            (values bytes #f)]))
       (if text?
           #"."
           (parameterize ([snipclass-bytes->image-separately-written-bytes separately-written-bytes-ht])
             (snipclass-bytes->image new-bts))))
     (super-new))))
