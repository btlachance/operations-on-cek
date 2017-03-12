#lang racket
(module db racket
  (require datalog)
  (provide lc)
  (define lc (make-theory))
  ;; TODO: i don't think subclass is the right word, since it makes
  ;; me think it should be transitive
  (datalog!
   lc
   
   (! (class x))
   (! (term default-var)) (! (subclass x default-var))

   ;; (! (class n))
   ;; (! (term natural)) (! (subclass n natural))

   (! (class v))
   (! (term lam)) (! (subclass v lam))
   ;; (! (term n)) (! (subclass v n))

   (! (class f))
   (! (term lam)) (! (subclass f lam))

   (! (class e))
   (! (term x)) (! (subclass e x)) ;; reads: x is a subclass of e
   (! (term app)) (! (subclass e app))
   (! (term lam)) (! (subclass e lam))
   ;; (! (term n)) (! (subclass e n))

   (! (:- (multiclass T)
          (subclass C1 T)
          (subclass C2 T)
          (!= C1 C2))))

  (module+ test
    (require rackunit)
    (check-equal? (length (datalog lc (? (multiclass lam)))) 1)
    (check-equal? (length (datalog lc (? (multiclass n)))) 1)
    (check-equal? (length (datalog lc (? (multiclass T)))) 2)))

(module+ main
  (require datalog (submod ".." db))

  (define (isevery? c1 c2)
    (define terms
      (flatten (map dict-values (datalog lc (? (subclass #,c1 T))))))
    (for/and ([t terms])
      (= (length (datalog lc (? (subclass #,c2 #,t)))) 1)))
  (define (multiclass-terms)
    (define terms
      (datalog
       lc
       (? (multiclass T))))
    (flatten (map dict-values terms)))
  
  (struct exn:fail:too-many-classes exn:fail
    (term classlist))
  (struct exn:fail:incompatible-classes exn:fail
    (term class1 class2))
  ;; demulticlass : term -> void
  ;; updates the database to ensure that t belongs to exactly one class
  ;; assumes t has is a subclass of >1 classes
  (define (demulticlass t)
    ;; all terms in from will be taken out of to and from will be made
    ;; a subclass of to
    (define-values (from to)
      (match (flatten (map dict-values (datalog lc (? (subclass C #,t)))))
        [(list c1 c2)
         (cond
           [(and (isevery? c1 c2) (not (isevery? c2 c1)))
            (values c1 c2)]
           [(and (isevery? c2 c1) (not (isevery? c1 c2)))
            (values c2 c1)]
           [else
            (raise (exn:fail:incompatible-classes
                    "incompatible classes"
                    (current-continuation-marks)
                    t c1 c2))])]
        [classes
         (raise (exn:fail:too-many-classes
                 "too many classes"
                 (current-continuation-marks)
                 t classes))]))
    (define from-terms
      (flatten (map dict-values (datalog lc (? (subclass #,from T))))))
    (for ([t from-terms])
      (datalog!
       lc
       (~ (subclass #,to #,t))))
    (datalog
     lc
     (! (subclass #,to #,from))))

  (define (demulticlass-all)
    ;; TODO: map terms/classes in exceptions to source-level names
    (let loop ()
      (define ts (multiclass-terms))
      (unless (zero? (length ts))
        (define t (first ts))
        (demulticlass t)
        (loop)))))

(module+ test
  (require (submod ".." db test)))
