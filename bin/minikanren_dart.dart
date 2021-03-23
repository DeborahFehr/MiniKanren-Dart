import 'package:minikanren_dart/minikanren_dart.dart';
import 'package:minikanren_dart/mk_interface.dart';

main(List<String> arguments) {
  Function nullo(dynamic x) => mEquals(x, []);
  Function conso(dynamic x, dynamic y, dynamic xy) => mEquals({x: y}, xy);
  Function appendo(dynamic l, dynamic t, dynamic out) {
    Map fit;
    List diff = [];
    if (l is MVar) {
      diff = out.toSet().difference(t.toSet()).toList();
      fit = {diff: t};
    } else if (t is MVar) {
      diff = out.toSet().difference(l.toSet()).toList();
      fit = {l: diff};
    } else {
      fit = out;
    }
    return conso(l, t, fit);
  }

  /*
  print(run_star('x', [nullo('x')])); // returns []
  print(run_star('x', [nullo(MVar('x'))])); // returns [[]]
  print(run_star('x', [nullo([])])); // returns ['_0']

  print(run_star('x', [
    conso(MVar('x'), 2, {1: 2})
  ])); // returns [1]

  print(run_star('x', [
    conso(1, MVar('x'), {1: 2})
  ])); // returns [2]

  print(run_star('x', [conso(1, 2, MVar('x'))])); // returns [[1, 2]]

  print(run_star('x', [
    conso(1, MVar('x'), {
      1: [2, 3]
    })
  ])); // returns [[2, 3]]
  */

  print("Appendo Example Example");
  print(run_star('x', [
    appendo([1, 2, 3], MVar('x'), [1, 2, 3, 4, 5, 6])
  ])); // returns [[4, 5, 6]]

// Usage: Teacupo Based on Frame 1:82 of the Reasoned Schemer
  Function teacupo(dynamic t) => disj([mEquals('tea', t), mEquals('cup', t)]);

  print("Teacupo Example");
  print(run_star('t', [teacupo(MVar('t'))])); // returns [tea, cup]
  print(run_star('t', [teacupo('tea')])); // returns [_0]
  print(run_star('t', [teacupo('pie')])); // returns []

// Programming Problem based on Chapter 7 of the Reasoned Schemer
// "A bit too much"

// bit-xoro based on Frame 7-5
  Function xoro(dynamic x, dynamic y, dynamic r) => condE([
        conj2(mEquals(0, x), mEquals(0, y)),
        mEquals(0, r),
        conj2(mEquals(0, x), mEquals(1, y)),
        mEquals(1, r),
        conj2(mEquals(1, x), mEquals(0, y)),
        mEquals(1, r),
        conj2(mEquals(1, x), mEquals(1, y)),
        mEquals(0, r)
      ]);

  print("Xoro Example");
  print(run_star('x', [xoro(MVar('x'), 0, 0)])); // returns [0]
  print(run_star('x', [xoro(MVar('x'), 0, 1)])); // returns [1]

// runstar (x,y) (bit-xoro x y 0) should return [[0,0], [1,1]]

// bit-xoro based on Frame 7-10
  Function ando(dynamic x, dynamic y, dynamic r) => condE([
        conj2(mEquals(0, x), mEquals(0, y)),
        mEquals(0, r),
        conj2(mEquals(0, x), mEquals(1, y)),
        mEquals(0, r),
        conj2(mEquals(1, x), mEquals(0, y)),
        mEquals(0, r),
        conj2(mEquals(1, x), mEquals(1, y)),
        mEquals(1, r)
      ]);

  print("Ando Example");
  print(run_star('x', [ando(MVar('x'), 0, 0)])); // returns [0, 1]
  print(run_star('x', [ando(MVar('x'), 0, 1)])); // returns []
  print(run_star('x', [ando(MVar('x'), 1, 1)])); // returns [1]
}

// TODO: ReadMe anpassen!
