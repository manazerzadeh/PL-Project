#lang racket
(require (lib "eopl.ss" "eopl"))

(define true? (lambda (true1) (eqv? true1 #t)))
(define posnum? (lambda (num1) (and (positive? num1) (number? num1))))
(define (scheme-val? x) #t)

(define (while condition body)
  (when (condition)
    (body)
    (while condition body)))



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
   (command2 command?)))

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




;value-of
(define 
(define value-of-command
  (lambda (command1 environment)
    (cases command command1
      (a-unitcom (unitcom1) (value-of-unitcom unitcom1 environment))
      (unitcoms (command2 unitcom1) (value-of-unitcom unitcom1 (value-of-command command2 environment)));(begin (value-of-command command2 environment) (value-of-unitcom unitcom1 environment)))
      )))

(define value-of-unitcom
  (lambda (unitcom1 environment)
    (cases unitcom unitcom1
      (while-com (while1) (value-of-whilecom while1 environment))
      (if-com (if1) (value-of-ifcom if1 environment))
      (assign-com (assign1) (value-of-assign assign1 environment))
      (return-com (return1) (value-of-return return1 environment)))))

;(define value-of-whilecom
 ; (lambda (whilecom1 environment)
  ;  (cases whilecom whilecom1
   ;   (while-statement (exp1 command1) (when (value-of-exp exp1 environment) (value-of-command command1 environment) (value-of-whilecom

(define value-of-ifcom
  (lambda (ifcom1 environment)
    (cases ifcom ifcom1
      (if-statement (exp1 command1 command2) (if (expval->bool (value-of-exp exp1 environment)) (value-of-command command1 environment) (value-of-command command2 environment))))))

(define value-of-assign
  (lambda (assign1 environment)
    (cases assign assign1
      (assignment (var1 exp1) (extend-env var1 (value-of-exp exp1 environment) environment)))))


(define value-of-return
  (lambda (return1 environment)
    (cases return return1
      (returnment (exp1) (begin (print (value-of-exp exp1 environment)) (exit))))))


(define value-of-exp
  (lambda (exp1 environment)
    (cases exp exp1
      (a-exp (aexp1) (value-of-aexp aexp1 environment))
      (more-exp (aexp1 aexp2) (let ( (val1 (value-of-aexp aexp1 environment)) (val2 (value-of-aexp aexp2 environment)) )
                                (cases expval val1
                                  (num-val (num1) (cases expval val2
                                                    (num-val (num2) (bool-val(> (expval->num num1) (expval->num num2))))
                                                    (list-val (list1) (bool-val (
      (less-exp (aexp1 aexp2) (< (value-of-aexp aexp1 environment) (value-of-aexp aexp2 environment)))
      (eq-exp (aexp1 aexp2) (= (value-of-aexp aexp1 environment) (value-of-aexp aexp2 environment)))
      (neq-exp (aexp1 aexp2) (not (= (value-of-aexp aexp1 environment) (value-of-aexp aexp2 environment))))
      )))

(define value-of-aexp
  (lambda (aexp1 environment)
    (cases aexp aexp1
      (b-aexp (bexp1) (value-of-bexp bexp1 environment))
      (diff-aexp (bexp1 aexp1) (- (value-of-bexp bexp1 environment) (value-of-aexp aexp1 environment)))
      (sum-aexp (bexp1 aexp1) (+ (value-of-bexp bexp1 environment) (value-of-aexp aexp1 environment)))
      )))

(define value-of-bexp
  (lambda (bexp1 environment)
    (cases bexp bexp1
      (c-bexp (cexp1) (value-of-cexp cexp1 environment))
      (mul-bexp (cexp1 bexp1) (* (value-of-cexp cexp1 environment) (value-of-bexp bexp1 environment)))
      (div-bexp (cexp1 bexp1) (/ (value-of-cexp cexp1 environment) (value-of-bexp bexp1 environment)))
      )))


      
      

;test
;(define a (extend-env 'x 2 (empty-env)))
;(apply-env a 'x)






