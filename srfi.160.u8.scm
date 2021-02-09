(declare (fixnum-arithmetic))

(module (srfi 160 u8) ()
  (import (scheme))
  (import (only (chicken base)
    open-input-string include define-record-type case-lambda
    when unless let-values))
  (import (only (chicken module) export))
  (import (only (chicken memory) move-memory!))
  (import (srfi 128))
  (import (srfi 160 base))

  ;; Constructors 
  (export make-u8vector u8vector
          u8vector-unfold u8vector-unfold-right
          u8vector-copy u8vector-reverse-copy 
          u8vector-append u8vector-concatenate
          u8vector-append-subvectors)
  ;; Predicates 
  (export u8? u8vector? u8vector-empty? u8vector=)
  ;; Selectors
  (export u8vector-ref u8vector-length)
  ;; Iteration 
  (export u8vector-take u8vector-take-right
          u8vector-drop u8vector-drop-right
          u8vector-segment
          u8vector-fold u8vector-fold-right
          u8vector-map u8vector-map! u8vector-for-each
          u8vector-count u8vector-cumulate)
  ;; Searching 
  (export u8vector-take-while u8vector-take-while-right
          u8vector-drop-while u8vector-drop-while-right
          u8vector-index u8vector-index-right u8vector-skip u8vector-skip-right 
          u8vector-any u8vector-every u8vector-partition
          u8vector-filter u8vector-remove)
  ;; Mutators 
  (export u8vector-set! u8vector-swap! u8vector-fill! u8vector-reverse!
          u8vector-copy! u8vector-reverse-copy!
          u8vector-unfold! u8vector-unfold-right!)
  ;; Conversion 
  (export u8vector->list reverse-u8vector->list reverse-list->u8vector
          list->u8vector u8vector->vector vector->u8vector)
  ;; Misc
  (export make-u8vector-generator u8vector-comparator write-u8vector)

  (include "srfi/160/u8-impl.scm")

  (define (eof-object)
     (let* ((p (open-input-string "")) (e (read p)))
      (close-input-port p) e))
)
