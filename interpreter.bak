#lang racket
(require (lib "eopl.ss" "eopl"))

(define true? (lambda (true1) (eqv? true1 #t)))
(define posnum? (lambda (num1) (and (positive? num1) (number? num1))))

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
  (while
   (exp1 exp?)
   (command1 command?)))

(define-datatype ifcom ifcom?
  (if
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






