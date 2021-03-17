//import 'package:minikanren_dart/minikanren_dart.dart' as minikanren_dart;

import 'package:minikanren_dart/minikanren_dart.dart';

//import '../lib/minikanren_dart.dart';

main(List<String> arguments) {
  print('Hello world:!');
  print(mEquals({2: MVar('x')}, {2: 3})(empty_s));
  print(mEquals({MVar('y'): 3}, {2: 3})(Substitution([
    {MVar('x'): 3}
  ])));

/*
Implementation of appendo
Racket Code:
(defrel (appendo l t out)
  (conde
   ((nullo l) (== t out))
   ((fresh (a d res)
           (conso a d l)
           (appendo d t res)
           (conso a res out)))))

with the following functions: 

(defrel (conso x xs xxs) (== (cons x xs) xxs))

(defrel (nullo x) (== '() x))

And Syntax Rules: 

(define-syntax defrel
  (syntax-rules ()
    ((defrel (name x ...) g ...)
     (define (name x ...)
       (lambda (s)
         (lambda ()
           ((conj g ...) s)))))))

(define-syntax conde
  (syntax-rules ()
    ((conde (g ...) ...)
     (disj (conj g ...) ...))))

(define-syntax run*
  (syntax-rules ()
    ((run* q g ...) (run #f q g ...))))

(define-syntax run
  (syntax-rules ()
    ((run n (x0 x ...) g ...)
     (run n q (fresh (x0 x ...)
                (== `(,x0 ,x ...) q) g ...)))
    ((run n q g ...)
     (let ((q (var 'q)))
       (map (reify q)
         (run-goal n (conj g ...)))))))

(run* (q) (appendo '(1 2 3) q '(1 2 3 4 5 6))

*/

// Kapitel 7 Seite 85:

// bit-or
// bit-xor
// bit-and
// bit-nand
}
