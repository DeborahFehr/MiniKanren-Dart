import 'package:collection/collection.dart';

// An MVar is a miniKanren Variable
class MVar {
  final String id;

  MVar(this.id);

  bool isEqual(MVar v) {
    return this.id == v.id;
  }

  @override
  String toString() {
    return this.id.toString();
  }

  @override
  bool operator ==(other) {
    if (other is MVar) {
      return this.id == other.id;
    }
    return false;
  }
}

bool isMVar(Object obj) {
  return (obj is MVar);
}

// An Association is a pair where the first Value is a
// MVar and the second one can be anything
// Association: Map<Var, Dynamic>

// A substitution is a list of associations
class Substitution {
  List<Map<MVar, dynamic>> associations = <Map<MVar, dynamic>>[];

  Substitution(List<Map<MVar, dynamic>> l) {
    this.associations = l;
  }

  // allows definition of empty list (extendable)
  Substitution.empty({growable = true});

  bool isEmpty() {
    return associations.isEmpty;
  }

  @override
  String toString() {
    return this.associations.toString();
  }

  Substitution.clone(Substitution sub) : this(new List.from(sub.associations));

  // override comparison to allow for comparisons of associations
  @override
  bool operator ==(other) {
    if (other is Substitution) {
      if (other.associations.length != this.associations.length) return false;
      bool result = true;
      for (int i = 0; i < this.associations.length; i++) {
        MVar thisKey = this.associations[i].keys.first;
        MVar otherKey = other.associations[i].keys.first;
        dynamic thisVal = this.associations[i].values.first;
        dynamic otherVal = other.associations[i].values.first;

        if (!thisKey.isEqual(otherKey)) result = false;
        if (isMVar(thisVal)) {
          if (!thisVal.isEqual(otherVal)) result = false;
        } else if (thisVal is Map || thisVal is List) {
          if (!DeepCollectionEquality().equals(thisVal, otherVal))
            result = false;
        } else {
          if (!(thisVal == otherVal)) result = false;
        }
      }
      return result;
    }
    return false;
  }
}

// empty-list constant, see report for details
final Substitution empty_s = Substitution.empty();

dynamic assv(MVar val, Substitution sub) {
  for (int i = 0; i < sub.associations.length; i++) {
    if (val.isEqual(sub.associations[i].keys.first)) return sub.associations[i];
  }
  return false;
}

dynamic walk(dynamic val, Substitution sub) {
  dynamic assoc = val;
  if (isMVar(val)) {
    assoc = assv(val, sub);
    if (assoc is Map) {
      var recursion = walk(assoc.values.first, sub);
      if (recursion == false) {
        return assoc.values.first;
      } else {
        return recursion;
      }
    } else {
      assoc = val;
    }
  }
  return assoc;
}

dynamic occurs(MVar key, dynamic val, Substitution sub) {
  var walkVal = walk(val, sub);
  if (isMVar(walkVal)) {
    return walkVal.isEqual(key);
  } else if (walkVal is Map) {
    return (occurs(key, walkVal.keys.first, sub) ||
        occurs(key, walkVal.values.first, sub));
  } else if (walkVal is List) {
    bool valInList = false;
    for (int i = 0; i < walkVal.length; i++) {
      if (occurs(key, walkVal.elementAt(i), sub)) {
        valInList = true;
      }
    }
    return valInList;
  }
  return false;
}

dynamic ext_s(MVar key, dynamic val, Substitution sub) {
  if (occurs(key, val, sub) == true) {
    return false;
  } else {
    List<Map<MVar, dynamic>> newSub = sub.associations;
    newSub.add({key: val});
    return Substitution(newSub);
  }
}

dynamic unify(dynamic u, dynamic v, Substitution sub) {
  var walkU = walk(u, sub);
  var walkV = walk(v, sub);

  if (walkU == walkV) return sub;
  // in Case we compare maps
  if (DeepCollectionEquality().equals(walkU, walkV)) return sub;
  if (isMVar(walkU)) return ext_s(walkU, walkV, sub);
  if (isMVar(walkV)) return ext_s(walkV, walkU, sub);
  if (walkU is Map && walkV is Map) {
    dynamic unifyCar = unify(walkU.keys.first, walkV.keys.first, sub);
    if (unifyCar != false) {
      return unify(walkU.values.first, walkV.values.first, unifyCar);
    }
  }
  if (walkU is Substitution && walkV is Substitution) {
    dynamic unifyCar = unify(walkU.associations[0].keys.first,
        walkV.associations[0].keys.first, sub);
    if (unifyCar != false) {
      return unify(walkU.associations[0].values.first,
          walkV.associations[0].values.first, unifyCar);
    }
  }

  return false;
}

// A stream is a list (empty list or list of substitutions)
// Stream: List<Substitution>
// A suspension is a function which returns a list of suspensions (stream)
// Suspension:  List<Substitution> Function ()

// A goal is a function that expects a substitution
// and returns a stream of substitutions
// Goal: List<Substitution> Function (Substitution sub)

Function s_succeed() {
  return (Substitution s) => [s];
}

Function u_fail() {
  return (Substitution s) => [];
}

Function mEquals(dynamic u, dynamic v) {
  return (Substitution s) {
    dynamic unifyResult = unify(u, v, s);
    if (unifyResult != false) {
      Substitution result = unifyResult;
      return [result];
    }
    return [];
  };
}

// no List<Substitution> as empty list can be passed
dynamic append_inf(List s, List t) {
  if (s.isEmpty) return t;
  if (s is List && s.isNotEmpty) {
    var recursion = append_inf(s.sublist(1), t);
    if (recursion is List && recursion.isEmpty) return [s.first];
    recursion.insert(0, s.first);
    return recursion;
  }
  return () => append_inf(t, s);
}

dynamic disj2(Function g1, dynamic g2) {
  return (Substitution s) {
    Substitution sCopy = Substitution.clone(s);
    if (g2 is List) return append_inf(g1(s), g2);
    return append_inf(g1(s), g2(sCopy));
  };
}

dynamic append_map_inf(Function g, dynamic s) {
  if (s is List && s.isEmpty) return [];
  if (s is List && s.isNotEmpty) {
    return append_inf(g(s.first), append_map_inf(g, s.sublist(1)));
  }
  return () => append_map_inf(g, [s]);
}

dynamic conj2(Function g1, Function g2) {
  return (Substitution s) => append_map_inf(g2, g1(s));
}

dynamic call_fresh(dynamic name, Function f) {
  return f(MVar(name));
}

dynamic reify_name(int n) {
  return "_" + n.toString();
}

dynamic walk_star(dynamic val, Substitution sub) {
  dynamic walkedVal = walk(val, sub);

  if (isMVar(walkedVal)) {
    return walkedVal;
  } else if (walkedVal is Map) {
    return [
      walk_star(walkedVal.keys.first, sub),
      walk_star(walkedVal.values.first, sub)
    ];
  } else if (walkedVal is List) {
    // dynamic list due to type inference
    List<dynamic> recursionList = List.from(walkedVal);
    for (int i = 0; i < walkedVal.length; i++) {
      if (isMVar(walkedVal[i])) {
        var recursion = walk_star(walkedVal[i], sub);
        if (recursion != false) {
          recursionList.removeAt(i);
          recursionList.insert(i, recursion);
        }
      }
    }
    return recursionList;
  }

  return walkedVal;
}

dynamic take_inf(dynamic n, dynamic s) {
  if ((n is int && n < 1) || (s is List && s.isEmpty)) return [];
  if (s is List && s.isNotEmpty) {
    if (n is int) n--;
    dynamic recursion = take_inf(n, s.sublist(1));
    if (recursion is List && recursion.isEmpty) return s.first;
    return [s.first, recursion];
  }
  return take_inf(n, [s]);
}

dynamic reify_s(dynamic val, Substitution reisub) {
  dynamic walkedVal = walk(val, reisub);

  if (isMVar(walkedVal)) {
    List<Map<MVar, dynamic>> newSub = reisub.associations;
    int length = reisub.associations.length;
    reisub.associations.length > 0 ? length = length - 1 : length = 0;
    newSub.add({val: reify_name(length)});
    return Substitution(newSub);
  } else if (walkedVal is Map) {
    return [
      reify_s(walkedVal.keys.first, reisub),
      reify_s(walkedVal.values.first, reisub)
    ];
  } else {
    return reisub;
  }
}

Function reify(dynamic val) {
  return (Substitution sub) {
    dynamic walkedVal = walk_star(val, sub);
    dynamic reifyVal = reify_s(walkedVal, Substitution.empty());
    return walk_star(walkedVal, reifyVal);
  };
}

dynamic run_goal(dynamic n, Function g) {
  return take_inf(n, g(Substitution.empty()));
}

dynamic ifte(Function g1, Function g2, Function g3) {
  return (Substitution sub) {
    Substitution sCopy = Substitution.clone(sub);
    var s_inf = g1(sub);
    if (s_inf == null || (s_inf is List && s_inf.isEmpty)) return g3(sCopy);
    if (s_inf is List && s_inf.isNotEmpty) return (append_map_inf(g2, s_inf));
    return s_inf;
  };
}
