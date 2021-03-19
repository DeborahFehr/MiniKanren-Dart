// This class implements the Appendix A: Connecting the wires

import 'minikanren_dart.dart';

Function disj(List<Function> input) {
  if (input.isEmpty) return u_fail;
  if (input.length == 1) return input[0];
  dynamic recursion = disj(input.sublist(1));
  return disj2(input[0], recursion);
}

Function conj(List<Function> input) {
  if (input.isEmpty) return s_succeed;
  if (input.length == 1) return input[0];
  return conj2(input[0], conj(input.sublist(1)));
}

Function defrel(List<Function> input) {
  return (Substitution sub) {
    return () => conj(input)(sub);
  };
/*
(define-syntax defrel
  (syntax-rules ()
    ((defrel (name x ...) g ...)
     (define (name x ...)
       (lambda (s)
         (lambda ()
           ((conj g ...) s)))))))
           */
}

dynamic condE(List<Function> input) {
  List<Function> conds = [];

  for (int i = 0; i < input.length; i = i + 2) {
    if (i < input.length - 1)
      conds.add(conj([input[i], input[i + 1]]));
    else
      conds.add(input[i]);
  }
  return disj(conds);
}

dynamic run_star(String q, List<Function> g) {
  //TODO: support multiple qs!
  return mRun(false, q, g);
}

dynamic mRun(dynamic n, String q, List<Function> g) {
  dynamic runGoal = run_goal(n, conj(g));
  MVar mQ = MVar(q);
  List result = [];
  if (runGoal is List && runGoal.isNotEmpty) {
    for (int i = 0; i < runGoal.length; i++) {
      result.add(reify(mQ)(runGoal[i]));
    }
  } else if (runGoal is Substitution) {
    result = [reify(mQ)(runGoal)];
  } else {
    result = [];
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

dynamic fresh(List<String> x, List<Function> g) {
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
