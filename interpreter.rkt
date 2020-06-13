#lang racket
(require (lib "eopl.ss" "eopl"))

(define true? (lambda (true1) (eqv? true1 #t)))
(define posnum? (lambda (num1) (and (positive? num1) (number? num1))))
(define (scheme-val? x) #t)
(define list-operator ( lambda (operator list1 num1)
                            (if (null? list1) '() (cons (operator (car list1) num1) (list-operator operator (cdr list1) num1)))))
(define list-operator2 ( lambda (operator list1 num1)
                            (if (null? list1) '() (cons (operator num1 (car list1)) (list-operator operator (cdr list1) num1)))))
(define list-or ( lambda (list1 num1)
                            (if (null? list1) '() (cons (or (car list1) num1) (list-or (cdr list1) num1)))))
(define list-and ( lambda (list1 num1)
                            (if (null? list1) '() (cons (and (car list1) num1) (list-and (cdr list1) num1)))))
(define neg-list (lambda (list1)
                   (if (null? list1) '() (cons (* -1 (car list1)) (neg-list (cdr list1))))))
(define inverse-list (lambda (list1)
                   (if (null? list1) '() (cons (/ 1 (car list1)) (inverse-list (cdr list1))))))


(define (while condition body)
  (when (condition)
    (body)
    (while condition body)))

(define listmem-extractor (lambda (list1 listmem1)
                            ( if (= 1 (length listmem1)) (list-ref list1 (car listmem1)) (listmem-extractor (list-ref list1 (car listmem1)) (cdr listmem1)))))


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

(define report-invalid-comparison
  (lambda ()
    (eopl:error "invalid comparison")))

(define report-invalid-operands
  (lambda ()
    (eopl:error "invalid operands")))

;define environment
(define-datatype env env?
  (empty-env)
  (extend-env
   (var1 symbol?)
   (val1 expval?)
   (env1 env?)))

(define apply-env
  (lambda (environment search-var)
    (cases env environment
      (empty-env () (report-no-binding-found search-var))
      (extend-env (var val saved-env) 
                                       (if (eqv? search-var var) val (apply-env saved-env search-var)))))) ;we may add report-bad-env
      
(define environment (empty-env))

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
   (listmem1 listmem?)))


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

(define expval->null
  (lambda (val)
    (cases expval val
      (null-val (null1) null1)
      (else (report-expval-extractor-error 'null val)))))




;value-of

(define value-of-command
  (lambda (command1)
    (cases command command1
      (a-unitcom (unitcom1) (value-of-unitcom unitcom1))
      (unitcoms (command2 unitcom1) (begin (value-of-command command2 ) (value-of-unitcom unitcom1 )))
      )))

(define value-of-unitcom
  (lambda (unitcom1)
    (cases unitcom unitcom1
      (while-com (while1) (value-of-whilecom while1))
      (if-com (if1) (value-of-ifcom if1))
      (assign-com (assign1) (value-of-assign assign1))
      (return-com (return1) (value-of-return return1)))))

(define value-of-whilecom
  (lambda (whilecom1)
    (cases whilecom whilecom1
      (while-statement (exp1 command1) (when (expval->bool (value-of-exp exp1)) (value-of-command command1) (value-of-whilecom (while-statement exp1 command1))))
      )))

(define value-of-ifcom
  (lambda (ifcom1)
    (cases ifcom ifcom1
      (if-statement (exp1 command1 command2) (if (expval->bool (value-of-exp exp1)) (value-of-command command1) (value-of-command command2))))))

(define value-of-assign
  (lambda (assign1)
    (cases assign assign1
      (assignment (var1 exp1) (extend-env var1 (value-of-exp exp1))))))


(define value-of-return
  (lambda (return1)
    (cases return return1
      (returnment (exp1) (begin (print (value-of-exp exp1)) (exit))))))


(define value-of-exp
  (lambda (exp1)
    (cases exp exp1
      (a-exp (aexp1) (value-of-aexp aexp1))
      (more-exp (aexp1 aexp2) (let ( (val1 (value-of-aexp aexp1)) (val2 (value-of-aexp aexp2)) )
                                (cases expval val1
                                  (num-val (num1) (cases expval val2
                                                    (num-val (num2) (bool-val(> (expval->num num1) (expval->num num2))))
                                                    (list-val (list1) (list-val (list-operator <= (expval->list list1) (expval->num num1))))
                                                    (else report-invalid-comparison)
                                                    ))
                                  (list-val (list1) (cases expval val2
                                                      (num-val (num1) (list-val (list-operator > (expval->list list1) (expval->num num1))))
                                                      (string-val (string1) (list-val (list-operator string>? (expval->list list1) (expval->string string1))))
                                                      (else report-invalid-comparison)))
                                  (string-val (string1) (cases expval val2
                                                          (list-val (list1) (list-val (list-operator string<=? (expval->list list1) (expval->string string1))))
                                                          (string-val (string2) (list-val (map string>? (expval->string string1) (expval->string string2))))
                                                          (else report-invalid-comparison)))
                                  (else report-invalid-comparison)
                                  )))
      (less-exp (aexp1 aexp2) (let ( (val1 (value-of-aexp aexp1)) (val2 (value-of-aexp aexp2)) )
                                (cases expval val1
                                  (num-val (num1) (cases expval val2
                                                    (num-val (num2) (bool-val(< (expval->num num1) (expval->num num2))))
                                                    (list-val (list1) (list-val (list-operator >= (expval->list list1) (expval->num num1))))
                                                    (else report-invalid-comparison)
                                                    ))
                                  (list-val (list1) (cases expval val2
                                                      (num-val (num1) (list-val (list-operator < (expval->list list1) (expval->num num1))))
                                                      (string-val (string1) (list-val (list-operator string<? (expval->list list1) (expval->string string1))))
                                                      (else report-invalid-comparison)))
                                  (string-val (string1) (cases expval val2
                                                          (list-val (list1) (list-val (list-operator string>=? (expval->list list1) (expval->string string1))))
                                                          (string-val (string2) (bool-val (string<? (expval->string string1) (expval->string string2))))
                                                          (else report-invalid-comparison)))
                                  (else report-invalid-comparison)

                               )))
      (eq-exp (aexp1 aexp2) (let ( (val1 (value-of-aexp aexp1)) (val2 (value-of-aexp aexp2)) )
                                (cases expval val1
                                  (num-val (num1) (cases expval val2
                                                    (num-val (num2) (bool-val(= (expval->num num1) (expval->num num2))))
                                                    (list-val (list1) (list-val (list-operator = (expval->list list1) (expval->num num1))))
                                                    (else report-invalid-comparison)
                                                    ))
                                  (list-val (list1) (cases expval val2
                                                      (num-val (num1) (list-val (list-operator = (expval->list list1) (expval->num num1))))
                                                      (string-val (string1) (list-val (list-operator string=? (expval->list list1) (expval->string string1))))
                                                      (bool-val (bool1)(list-val (list-operator eqv? (expval->list list1) (expval->bool bool1))))
                                                      (null-val (null1)(list-val (list-operator eqv? (expval->list list1) (expval->null null1))))
                                                      (list-val (list2) (list-val (map eqv? (expval->list list1) (expval->list list2)))) 
                                                      ))
                                  (string-val (string1) (cases expval val2
                                                          (list-val (list1) (list-val (list-operator string=? (expval->list list1) (expval->string string1))))
                                                          (string-val (string2) (bool-val (string=? (expval->string string1) (expval->string string2))))
                                                          (else report-invalid-comparison)))
                                  (bool-val (bool1) (cases expval val2
                                                      (list-val (list1) (list-val (list-operator eqv? (expval->list list1) (expval->bool bool1))))
                                                      (bool-val (bool2) (bool-val (eqv? (expval->bool bool1) (expval->bool bool2))))
                                                      (else report-invalid-comparison)))
                                  (null-val (null1) (cases expval val2
                                                      (null-val (null2) (bool-val #t))
                                                      (list-val (list1) (list-val (list-operator eqv? (expval->list list1) (expval->null null1))))
                                                      (else report-invalid-comparison)))
                                  )))
      (neq-exp (aexp1 aexp2)   (let ( (val1 (value-of-aexp aexp1)) (val2 (value-of-aexp aexp2)) )
                                (cases expval val1
                                  (num-val (num1) (cases expval val2
                                                    (num-val (num2) (bool-val(not (= (expval->num num1) (expval->num num2)))))
                                                    (list-val (list1) (list-val (map not (list-operator = (expval->list list1) (expval->num num1)))))
                                                    (else report-invalid-comparison)
                                                    ))
                                  (list-val (list1) (cases expval val2
                                                      (num-val (num1) (list-val (map not (list-operator = (expval->list list1) (expval->num num1)))))
                                                      (string-val (string1) (list-val (map not (list-operator string=? (expval->list list1) (expval->string string1)))))
                                                      (bool-val (bool1)(list-val (map not (list-operator eqv? (expval->list list1) (expval->bool bool1)))))
                                                      (null-val (null1)(list-val (map not (list-operator eqv? (expval->list list1) (expval->null null1)))))
                                                      (list-val (list2) (list-val (map not (map eqv? (expval->list list1) (expval->list list2)))))
                                                      ))
                                  (string-val (string1) (cases expval val2
                                                          (list-val (list1) (list-val (map not (list-operator string=? (expval->list list1) (expval->string string1)))))
                                                          (string-val (string2) (bool-val (not (string=? (expval->string string1) (expval->string string2)))))
                                                          (else report-invalid-comparison)))
                                  (bool-val (bool1) (cases expval val2
                                                      (list-val (list1) (list-val (map not (list-operator eqv? (expval->list list1) (expval->bool bool1)))))
                                                      (bool-val (bool2) (bool-val (not (eqv? (expval->bool bool1) (expval->bool bool2)))))
                                                      (else report-invalid-comparison)))
                                  (null-val (null1) (cases expval val2
                                                      (null-val (null2) (bool-val #f))
                                                      (list-val (list1) (list-val (map not (list-operator eqv? (expval->list list1) (expval->null null1)))))
                                                      (else report-invalid-comparison)))
                                  )))
      )))

(define value-of-aexp
  (lambda (aexp1)
    (cases aexp aexp1
      (b-aexp (bexp1) (value-of-bexp bexp1))
      (diff-aexp (bexp1 aexp1) (let( (val1 (value-of-bexp bexp1)) (val2 (value-of-aexp aexp1)) )
                                 (cases expval val1
                                   (num-val (num1) (cases expval val2
                                                     (num-val (num2) (num-val (- (expval->num num1) (expval->num num2))))
                                                     (list-val (list1) (list-val (neg-list (list-operator - (expval->list list1) (expval->num num1)))))
                                                     (else report-invalid-operands)))
                                   (list-val (list1) (cases expval val2
                                                       (num-val (num1) (list-val (list-operator - (expval->list list1) (expval->num num1))))
                                                       (else report-invalid-operands)))
                                   (else report-invalid-operands)
                                   )))
      (sum-aexp (bexp1 aexp1) (let( (val1 (value-of-bexp bexp1)) (val2 (value-of-aexp aexp1))  )
                                (cases expval val1
                                  (num-val (num1) (cases expval val2
                                                    (num-val (num2) (num-val (+ (expval->num num1) (expval->num num2))))
                                                    (list-val (list1) (list-val (list-operator + (expval->list list1) (expval->num num1))))
                                                    (else report-invalid-operands)))
                                  (list-val (list1) (cases expval val2
                                                      (num-val (num1) (list-val (list-operator + (expval->list list1) (expval->num num1))))
                                                      (list-val (list2) (list-val (append (expval->list list1) (expval->list list2))))
                                                      (bool-val (bool1) (list-val (list-or (expval->list list1) (expval->bool bool1))))
                                                      (string-val (string1) (list-val (list-operator string-append (expval->list list1) (expval->string string1))))
                                                      (else report-invalid-operands)))
                                  (bool-val (bool1) (cases expval val2
                                                      (bool-val (bool2) (bool-val (or (expval->bool bool1) (expval->bool bool2))))
                                                      (list-val (list1) (list-val (list-or (expval->list list1) (expval->bool bool1))))
                                                      (else report-invalid-operands)))
                                  (string-val (string1) (cases expval val2
                                                          (string-val (string2) (string-val (string-append (expval->string string1) (expval->string string2))))
                                                          (list-val (list1) (list-val (list-operator string-append (expval->list list1) (expval->string string1))))
                                                          (else report-invalid-operands)))
                                  (else report-invalid-operands)
                                  )))
      )))

(define value-of-bexp
  (lambda (bexp1)
    (cases bexp bexp1
      (c-bexp (cexp1) (value-of-cexp cexp1))
      (mul-bexp (cexp1 bexp1) (let( (val1 (value-of-cexp cexp1)) (val2 (value-of-bexp bexp1))  )
                                (cases expval val1
                                  (num-val (num1) (cases expval val2
                                                    (num-val (num2) (num-val (* (expval->num num1) (expval->num num2))))
                                                    (list-val (list1) (list-val (list-operator * (expval->list list1) (expval->num num1))))
                                                    (else report-invalid-operands)))
                                  (list-val (list1) (cases expval val2
                                                      (num-val (num1) (list-val (list-operator * (expval->list list1) (expval->num num1))))
                                                      (bool-val (bool1) (list-val (list-and (expval->list list1) (expval->bool bool1))))
                                                      (else report-invalid-operands)))
                                  (bool-val (bool1) (cases expval val2
                                                      (bool-val (bool2) (bool-val (and (expval->bool bool1) (expval->bool bool2))))
                                                      (list-val (list1) (list-val (list-and (expval->list list1) (expval->bool bool1))))
                                                      (else report-invalid-operands)))
                                  (else report-invalid-operands)
                                  )))

      (div-bexp (cexp1 bexp1) (let( (val1 (value-of-cexp cexp1)) (val2 (value-of-bexp bexp1)) )
                                 (cases expval val1
                                   (num-val (num1) (cases expval val2
                                                     (num-val (num2) (num-val (/ (expval->num num1) (expval->num num2))))
                                                     (list-val (list1) (list-val (inverse-list (list-operator / (expval->list list1) (expval->num num1)))))
                                                     (else report-invalid-operands)))
                                   (list-val (list1) (cases expval val2
                                                       (num-val (num1) (list-val (list-operator / (expval->list list1) (expval->num num1))))
                                                       (else report-invalid-operands)))
                                   (else report-invalid-operands)
                                   )))
                
      )))

(define value-of-cexp
  (lambda (cexp1)
    (cases cexp cexp1
      (neg-cexp (nexp1) (let ( (val1 value-of-cexp) )
                          (cases expval val1
                            (num-val (num1) (num-val (* -1 (expval->num val1))))
                            (bool-val (bool1) (bool-val (not (expval->bool bool1))))
                            (list-val (list1) (list-val (list-operator * (expval->list list1) -1)))
                            (else report-invalid-operands)
                            )))
      (par-cexp (exp1) (value-of-exp exp1))
      (num-cexp (num1) (num-val num1))
      (null-cexp (null1) (null-val null1))
      (var-cexp (var1) (apply-env environment var1))
      (true-cexp (true1) (bool-val #t))
      (false-cexp (false1) (bool-val #f))
      (string-cexp (string1) (string-val string1))
      (list-cexp (list1) (value-of-newlist list1))
      (listmem-cexp (var1 listmem1) (listmem-extractor (expval->list(apply-env environment var1)) (expval->list (value-of-listmem listmem1))))
      )))

(define value-of-newlist
  (lambda (newlist1)
    (cases newlist newlist1
      (empty-newlist () (list-val '()))
      (list-newlist (listvalues1) (value-of-listvalues))
      )))

(define value-of-listvalues
  (lambda (listvalues1)
    (cases listvalues listvalues1
      (exp-listvalues (exp1) (list (value-of-exp exp1)))
      (append-listvalues (exp1 listvalues2) (list-val (cons (value-of-exp exp1) (expval->list (value-of-listvalues listvalues2))))) ;fix value-of-exp exp1
      )))


(define value-of-listmem
  (lambda (listmem1)
    (cases listmem listmem1
      (exp-listmem (exp1) (list-val (list (expval->num (value-of-exp exp1)))))
      (append-listmem (exp1 listmem1) (list-val (cons (expval->num (value-of-exp exp1)) (expval->list (value-of-listmem listmem1)))))
      )))


      
      

;test
;(define a (extend-env 'x 2 (empty-env)))
;(apply-env a 'x)






