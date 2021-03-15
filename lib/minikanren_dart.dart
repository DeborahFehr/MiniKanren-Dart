import 'package:collection/collection.dart';

// Implementation of Variable
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
// Variable and the second one can be anything
// We implement this as a Map<Var, Dynamic>

// A substitution is a list of associations
class Substitution {
  // define List Type here
  List<Map<MVar, dynamic>> associations =
      <Map<MVar, dynamic>>[]; // create List of Associations

  // TODO: Frame XYZ: cannot containt 2 associations with same
  // key, so check this via occurs
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

  @override
  bool operator ==(other) {
    if (other is Substitution) {
      if (other.associations.length != this.associations.length) return false;
      bool result = true;
      for (int i = 0; i < this.associations.length; i++) {
        MVar thisKey = this.associations[i].keys.elementAt(0);
        MVar otherKey = other.associations[i].keys.elementAt(0);
        dynamic thisVal = this.associations[i].values.elementAt(0);
        dynamic otherVal = other.associations[i].values.elementAt(0);

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

// empty-list constant
Substitution empty_s = Substitution.empty();

// returns the association pair
dynamic assv(MVar val, Substitution sub) {
  for (int i = 0; i < sub.associations.length; i++) {
    if (val.isEqual(sub.associations[i].keys.elementAt(0)))
      return sub.associations[i];
  }
  return false;
}

/// Returns the value of an association
dynamic walk(dynamic val, Substitution sub) {
  dynamic assoc = val;
  if (isMVar(val)) {
    assoc = assv(val, sub);
    if (assoc is Map) {
      var recursion = walk(assoc.values.elementAt(0), sub);
      if (recursion == false) {
        return assoc.values.elementAt(0);
      } else {
        return recursion;
      }
    } else {
      assoc = val;
    }
  }
  return assoc;
}

// checks if a cycle could appear in the substitution
dynamic occurs(MVar key, dynamic val, Substitution sub) {
  var walkVal = walk(val, sub);
  if (isMVar(walkVal)) {
    return walkVal.isEqual(key);
  } else if (walkVal is Map) {
    return (occurs(key, walkVal.keys.elementAt(0), sub) ||
        occurs(key, walkVal.values.elementAt(0), sub));
  }
  // If val is a list, check all List values (compare Frame XYZ)
  else if (walkVal is List) {
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

// extends the Substitution with a new Association
dynamic ext_s(MVar key, dynamic val, Substitution sub) {
  if (occurs(key, val, sub) == true) {
    return false;
  } else {
    List<Map<MVar, dynamic>> newSub = sub.associations;
    newSub.add({key: val});
    return Substitution(newSub);
  }
}

// unifies two lists of substitutions
dynamic unify(dynamic u, dynamic v, Substitution sub) {
  var walkU = walk(u, sub);
  var walkV = walk(v, sub);

  if (walkU == walkV) return sub;
  // in Case we compare maps
  if (DeepCollectionEquality().equals(walkU, walkV)) return sub;
  if (isMVar(walkU)) return ext_s(walkU, walkV, sub);
  if (isMVar(walkV)) return ext_s(walkV, walkU, sub);
  if (walkU is Map && walkV is Map) {
    dynamic unifyCar =
        unify(walkU.keys.elementAt(0), walkV.keys.elementAt(0), sub);
    if (unifyCar != false) {
      return unify(
          walkU.values.elementAt(0), walkV.values.elementAt(0), unifyCar);
    }
  }
  if (walkU is Substitution && walkV is Substitution) {
    dynamic unifyCar = unify(walkU.associations[0].keys.elementAt(0),
        walkV.associations[0].keys.elementAt(0), sub);
    if (unifyCar != false) {
      return unify(walkU.associations[0].values.elementAt(0),
          walkV.associations[0].values.elementAt(0), unifyCar);
    }
  }

  return false;
}

// A stream is a list (empty list or list of substitutions)
// A suspension is a function which returns a list of suspensions (stream)

// A goal is a function that expects a substitution
// and returns a stream of substitutions

// https://www.tutorialspoint.com/dart_programming/dart_programming_typedef.htm
// typedef Goal = MkStream Function(State sc);

Function s_succeed() {
  return (Substitution s) => [s];
}

Function u_fail() {
  return (Substitution s) => [];
}

Function mEquals(dynamic u, dynamic v) {
  dynamic unifyResult = unify(u, v, Substitution.empty());
  if (unifyResult != false) {
    return (s) => [unifyResult];
  }
  return (s) => [];
}

// append-inf produces a stream
dynamic append_inf(dynamic s, dynamic t) {
  if (t == null) return t;
  if (s is List) {
    return [s.elementAt(0), append_inf(s.removeAt(0), t)];
  } else {
    return () => append_inf(t, s);
  }

/*
(define (append-inf s-inf t-inf)
  (cond
    ((null? s-inf) t-inf)
    ((pair? s-inf) 
     (cons (car s-inf)
       (append-inf (cdr s-inf) t-inf)))
    (else (lambda () 
            (append-inf t-inf (s-inf))))))
*/
}

// models or and returns a stream
dynamic disj2(dynamic g1, dynamic g2) {
  return (s) => append_inf([g1, s], [g2, s]);
/*
(define (disj2 g1 g2)
  (lambda (s)
    (append-inf (g1 s) (g2 s))))
*/
}

//
dynamic append_map_inf(dynamic g, dynamic s) {
  if (s == null) return s;
  if (s is List) {
    return append_inf([g, s.elementAt(0)], append_map_inf(g, s.removeAt(0)));
  } else {
    return () => append_inf(g, [s]);
  }

/*
(define (append-map-inf g s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf)
     (append-inf (g (car s-inf))
       (append-map-inf g (cdr s-inf))))
    (else (lambda () 
            (append-map-inf g (s-inf))))))
*/
}

// models and and returns a stream
dynamic conj2(dynamic g1, dynamic g2) {
  return (s) => append_map_inf(g2, [g1, s]);
/*
(define (conj2 g1 g2)
  (lambda (s)
    (append-map-inf g2 (g1 s))))
*/
}

//
dynamic call_fresh(dynamic name, Function f) {
  return f(MVar(name));
/*
(define (call/fresh name f)
  (f (var name)))
*/
}

// returns reified value (underscore natural number)
// strings are naturally immutable in dart
dynamic reify_name(int n) {
  return "_" + n.toString();
/*
(define (reify-name n)
  (string->symbol
    (string-append "_"
      (number->string n))))
*/
}

//
dynamic walk_star(dynamic val, Substitution sub) {
  dynamic walkedVal = walk(val, sub);

  if (isMVar(walkedVal)) {
    return walkedVal;
  } else if (walkedVal is Map) {
    return [
      walk_star(walkedVal.keys.first, sub),
      walk_star(walkedVal.values.first, sub)
    ];
  } else {
    return walkedVal;
  }

/*
(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v)
       (cons
         (walk* (car v) s)
         (walk* (cdr v) s)))
      (else v))))
*/
}

//
dynamic reify_s(dynamic val, Substitution reisub) {
  dynamic walkedVal = walk(val, reisub);

  if (isMVar(walkedVal)) {
    return {
      val: {reify_name(reisub.associations.length): reisub.associations.length}
    };
  } else if (walkedVal is Map) {
    return [
      reify_s(walkedVal.keys.first, reisub),
      reify_s(walkedVal.values.first, reisub)
    ];
  } else {
    return reisub;
  }
/*
(define (reify-s v r)
  (let ((v (walk v r)))
    (cond
      ((var? v)
       (let ((n (length r)))
         (let ((rn (reify-name n)))
           (cons `(,v . ,rn) r))))
      ((pair? v)
       (let ((r (reify-s (car v) r)))
         (reify-s (cdr v) r)))
      (else r))))
*/
}

//
Function reify(dynamic val) {
  return (Substitution sub) {
    dynamic walkedVal = walk(val, sub);
    return walk_star(walkedVal, reify_s(walkedVal, empty_s));
  };
/*
(define (reify v)
  (lambda (s)
    (let ((v (walk* v s)))
      (let ((r (reify-s v empty-s)))
        (walk* v r)))))
*/
}

//
Function take_inf(dynamic n, dynamic val) {
  return (Substitution sub) {
    dynamic walkedVal = walk(val, sub);
    return walk_star(walkedVal, reify_s(walkedVal, empty_s));
  };
/*
(define (take-inf n s-inf)
  (cond
    ((and n (zero? n)) '())
    ((null? s-inf) '())
    ((pair? s-inf) 
     (cons (car s-inf)
       (take-inf (and n (sub1 n))
         (cdr s-inf))))
    (else (take-inf n (s-inf)))))
*/
}

//
dynamic run_goal(dynamic g1, dynamic g2) {
/*
(define (run-goal n g)
  (take-inf n (g empty-s)))
*/
}

//
dynamic ifte(dynamic g1, dynamic g2, dynamic g3) {
/*
(define (ifte g1 g2 g3)
  (lambda (s)
    (let loop ((s-inf (g1 s)))
      (cond
        ((null? s-inf) (g3 s))
        ((pair? s-inf)
         (append-map-inf g2 s-inf))
        (else (lambda ()
                (loop (s-inf))))))))
*/
}
