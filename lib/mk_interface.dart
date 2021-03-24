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

dynamic run_star(List<String> q, List<Function> g) {
  return mRun(false, q, g);
}

dynamic mRun(dynamic n, List<String> q, List<Function> g) {
  dynamic runGoal = run_goal(n, conj(g));
  List result = [];
  for (int i = 0; i < q.length; i++) {
    MVar mQ = MVar(q[i]);
    if (runGoal is List && runGoal.isNotEmpty) {
      List interim = [];
      for (int x = 0; x < runGoal.length; x++) {
        interim.add(reify(mQ)(runGoal[x]));
      }
      if (q.length > 1)
        result.add(interim);
      else
        result = interim;
    } else if (runGoal is Substitution) {
      result.add(reify(mQ)(runGoal));
    }
  }
  return result;
}

dynamic fresh(List<String> x, List<Function> g) {
  if (x.isEmpty) return conj(g);
  return call_fresh(x[0], (var x0) => fresh(x.sublist(1), g));
}
