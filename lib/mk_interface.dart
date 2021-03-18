// This class implements the Appendix A: Connecting the wires

import 'minikanren_dart.dart';

Function disj(List<Function> input) {
  if (input.isEmpty) return u_fail;
  if (input.length == 1) return input[0];
  return disj2(input[0], disj(input.sublist(1)));
/*
(define-syntax disj
  (syntax-rules ()
    ((disj) fail)
    ((disj g) g)
    ((disj g0 g ...) (disj2 g0 (disj g ...)))))
*/
}

Function conj(List<Function> input) {
  if (input.isEmpty) return s_succeed;
  if (input.length == 1) return input[0];
  return conj2(input[0], conj(input.sublist(1)));
  /*
(define-syntax conj
  (syntax-rules ()
    ((conj) succeed)
    ((conj g) g)
    ((conj g0 g ...) (conj2 g0 (conj g ...)))))
    */
}

Function defrel(List<Function> input) {
  return (Substitution sub) {
    return () => conj(input)(sub);
  };
/*
(defrel (conso x xs xxs) (== (cons x xs) xxs))
(define-syntax defrel
  (syntax-rules ()
    ((defrel (name x ...) g ...)
     (define (name x ...)
       (lambda (s)
         (lambda ()
           ((conj g ...) s)))))))
           */
}

dynamic condE(List<Function> input, List<Function> output) {
  return disj([conj(input)]); // TODO Add Output?
/*
(define-syntax conde
  (syntax-rules ()
    ((conde (g ...) ...)
     (disj (conj g ...) ...))))
*/
}

dynamic run_star(String q, List<Function> g) {
  return mRun(false, q, g);
  /*
(define-syntax run*
  (syntax-rules ()
    ((run* q g ...) (run #f q g ...))))
*/
}

dynamic mRun(dynamic n, String q, List<Function> g) {
  final runGoal = run_goal(n, conj(g));
  MVar mQ = MVar(q);
  List result;
  for (int i = 0; i < runGoal.length; i++) {
    result[i] = reify(mQ)(runGoal[i]);
  }
  return result;
/*
(define-syntax run
  (syntax-rules ()
    ((run n (x0 x ...) g ...)
     (run n q (fresh (x0 x ...)
                (== `(,x0 ,x ...) q) g ...)))
    ((run n q g ...)
     (let ((q (var 'q)))
       (map (reify q)
         (run-goal n (conj g ...)))))))
*/
}

dynamic fresh(List<dynamic> x, List<Function> g) {
  if (x.isEmpty) return conj(g);
  return call_fresh(x[0], (var x0) => fresh(x.sublist(1), g));
  /*
(define-syntax fresh
  (syntax-rules ()
    ((fresh () g ...) (conj g ...))
    ((fresh (x0 x ...) g ...)
     (call/fresh 'x_0
       (lambda (x0)
         (fresh (x ...) g ...))))))
         */
}
