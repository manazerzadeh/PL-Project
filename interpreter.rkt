#lang racket
(require (lib "eopl.ss" "eopl"))

(define true? (lambda (true1) (eqv? true1 #t)))
(define posnum? (lambda (num1) (and (positive? num1) (number? num1))))
(define (scheme-val? x) #t)



;define reports
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "NO binding for ~s" search-var)))


(define report-expval-extractor-error
  (lambda (sym var)
    (case (sym)
      [('num) (eopl:error 'expval->num "~s is not a number" var)]
      [('bool)(eopl:error 'expval->bool "~s is not a boolean" var)]
      [('list)(eopl:error 'expval->list "~s is not a list" var)]
      [('string)(eopl:error 'expval->string "~s is not a string" var)]
      )))

;define environment
(define-datatype env env?
  (empty-env)
  (extend-env
   (var1 symbol?)
   (val1 scheme-val?)
   (env1 env?)))

(define apply-env
  (lambda (environment search-var)
    (cases env environment
      (empty-env () (report-no-binding-found search-var))
      (extend-env (var val saved-env) 
                                       (if (eqv? search-var var) val (apply-env saved-env search-var)))))) ;we may add report-bad-env
      


;define grammer of the language and constructors
(define-datatype command command?
  (a-unitcom
   (unitcom1 unitcom?))
  (unitcoms
   (command1 command?)
   (unitcom1 unitcom?) ) )

(define-datatype unitcom unitcom?
  (while-com
   (while1 whilecom?))
  (if-com
   (if1 ifcom?))
  (assign-com
   (assign1 assign?))
  (return-com
   (return1 return?)))

(define-datatype whilecom whilecom?
  (while-statement
   (exp1 exp?)
   (command1 command?)))


(define-datatype ifcom ifcom?
  (if-statement
   (exp1 exp?)
   (command1 command?)
   (command command?)))

(define-datatype assign assign?
  (assignment
   (var1 symbol?)
   (exp1 exp?)))

(define-datatype return return?
  (returnment
   (exp1 exp?)))

(define-datatype exp exp?
  (a-exp
   (aexp1 aexp?))
  (more-exp
   (aexp1 aexp?)
   (aexp2 aexp?))
  (less-exp
   (aexp1 aexp?)
   (aexp2 aexp?))
  (eq-exp
   (aexp1 aexp?)
   (aexp2 aexp?))
  (neq-exp
   (aexp1 aexp?)
   (aexp2 aexp?)))

(define-datatype aexp aexp?
  (b-aexp
   (bexp1 bexp?))
  (diff-aexp
   (bexp1 bexp?)
   (aexp1 aexp?))
  (sum-aexp
   (bexp1 bexp?)
   (aexp1 aexp?)))

(define-datatype bexp bexp?
  (c-bexp
   (cexp1 cexp?))
  (mul-bexp
   (cexp1 cexp?)
   (bexp1 bexp?))
  (div-bexp
   (cexp1 cexp?)
   (bexp1 bexp?)))

(define-datatype cexp cexp?
  (neg-cexp
   (cexp1 cexp?))
  (par-cexp
   (exp1 exp?))
  (num-cexp
   (num1 posnum?))
  (null-cexp
   (null1 null?))
  (var-cexp
   (var1 symbol?))
  (true-cexp
   (true1 true?))
  (false-cexp
   (false1 false?))
  (string-cexp
   (string1 string?))
  (list-cexp
   (list1 newlist?))
  (listmem-cexp
   (var1 symbol?)
   (listmem1 listmem?)))

(define-datatype newlist newlist?
  (empty-newlist)
  (list-newlist
   (listvalues1 listvalues?)))

  
(define-datatype listvalues listvalues?
  (exp-listvalues
   (exp1 exp?))
  (append-listvalues
   (exp1 exp?)
   (listvalues1 listvalues?)))

(define-datatype listmem listmem?
  (exp-listmem
   (exp1 exp?))
  (append-listmem
   (exp1 exp?)
   (listmem listmem?)))


;define expression value sets
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (null-val
   (null1 null?))
  (list-val
   (list1 list?))
  (string-val
   (string1 string?)))

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error 'num val)))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error 'bool val)))))

(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (list1) list1)
      (else (report-expval-extractor-error 'list val)))))

(define expval->string
  (lambda (val)
    (cases expval val
      (string-val (string1) string1)
      (else (report-expval-extractor-error 'string val)))))




;test
;(define a (extend-env 'x 2 (empty-env)))
;(apply-env a 'x)






