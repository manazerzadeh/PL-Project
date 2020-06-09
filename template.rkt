#lang racket



(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(require (lib "eopl.ss" "eopl")) 




;I define the grammer here. use constructors like 'num-exp' to produce abstract syntax tree
(define-datatype exp exp?
  (num-exp
   (num1 number?))
  (sum-exp
   (exp1 exp?)
   (num1 number?)))


;I define interpreter here
(define value-of
  (lambda (program)
    (cases exp program
      (num-exp (num1) num1)
      (sum-exp (exp1 num1) (+ (value-of exp1) num1))
      )))

;scanner 
(define simple-math-lexer
           (lexer
            ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUM (string->number lexeme)))
            ("+" (token-plus))
            (whitespace (simple-math-lexer input-port))
            ((eof) (token-EOF))))

(define-tokens a (NUM))
(define-empty-tokens b (EOF plus))



;parser
(define simple-math-parser
           (parser
            (start exp)
            (end EOF)
            (error void)
            (tokens a b)
            (grammar
           (exp ((exp plus NUM) (sum-exp $1 $3)) ((NUM) (num-exp $1))) ;here I used defined constructors instead of the provided example. Compare to see the differences.
             )))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "1+2+ 3 +   4")))
(define output (let ((parser-res (simple-math-parser my-lexer))) parser-res)) ;output has the final value of parser
(value-of output) ;result of interpreter


