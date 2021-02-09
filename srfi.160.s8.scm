(declare (fixnum-arithmetic))

(module (srfi 160 s8) ()
  (import (scheme))
  (import (only (chicken base)
    open-input-string include define-record-type case-lambda
    when unless let-values))
  (import (only (chicken module) export))
  (import (only (chicken memory) move-memory!))
  (import (srfi 128))
  (import (srfi 160 base))

  ;; Constructors 
  (export make-s8vector s8vector
          s8vector-unfold s8vector-unfold-right
          s8vector-copy s8vector-reverse-copy 
          s8vector-append s8vector-concatenate
          s8vector-append-subvectors)
  ;; Predicates 
  (export s8? s8vector? s8vector-empty? s8vector=)
  ;; Selectors
  (export s8vector-ref s8vector-length)
  ;; Iteration 
  (export s8vector-take s8vector-take-right
          s8vector-drop s8vector-drop-right
          s8vector-segment
          s8vector-fold s8vector-fold-right
          s8vector-map s8vector-map! s8vector-for-each
          s8vector-count s8vector-cumulate)
  ;; Searching 
  (export s8vector-take-while s8vector-take-while-right
          s8vector-drop-while s8vector-drop-while-right
          s8vector-index s8vector-index-right s8vector-skip s8vector-skip-right 
          s8vector-any s8vector-every s8vector-partition
          s8vector-filter s8vector-remove)
  ;; Mutators 
  (export s8vector-set! s8vector-swap! s8vector-fill! s8vector-reverse!
          s8vector-copy! s8vector-reverse-copy!
          s8vector-unfold! s8vector-unfold-right!)
  ;; Conversion 
  (export s8vector->list reverse-s8vector->list reverse-list->s8vector
          list->s8vector s8vector->vector vector->s8vector)
  ;; Misc
  (export make-s8vector-generator s8vector-comparator write-s8vector)

  (include "srfi/160/s8-impl.scm")

  (define (eof-object)
     (let* ((p (open-input-string "")) (e (read p)))
      (close-input-port p) e))
)
