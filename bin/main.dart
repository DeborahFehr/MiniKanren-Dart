//import 'package:minikanren_dart/minikanren_dart.dart' as minikanren_dart;

import 'package:minikanren_dart/minikanren_dart.dart';
import 'package:minikanren_dart/mk_interface.dart';

//import '../lib/minikanren_dart.dart';

main(List<String> arguments) {
  Function nullo(dynamic x) => defrel([mEquals([], x)]);
  Function conso(dynamic x, dynamic y, dynamic xy) => defrel([
        mEquals([x, y], xy)
      ]);
  Function appendo(dynamic l, dynamic t, dynamic out) => defrel([
        condE([
          nullo(l),
          fresh([
            'a',
            'd',
            'res'
          ], [
            conso('a', 'd', l),
            appendo('d', t, 'res'),
            conso('a', 'res', out)
          ])
        ], [
          mEquals(t, out)
        ])
      ]);

  print(run_star('q', [
    appendo([1, 2, 3], 'q', [1, 2, 3, 4, 5, 6])
  ]));

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

(run* (q) (appendo '(1 2 3) q '(1 2 3 4 5 6))

*/

// Implementation Chapter 7 bit-XOR

Function bit_xoro(int x, int y, int r){
  condE([mEquals(x, 0),mEquals(x, 1)], [mEquals(r, 0), mEquals(r, 1)])
}

// runstar (x,y) (bit-xoro x y 0) should return [[0,0], [1,1]]

}
