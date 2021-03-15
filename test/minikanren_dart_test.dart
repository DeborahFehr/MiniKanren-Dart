import 'package:minikanren_dart/minikanren_dart.dart';
import 'package:test/test.dart';

//import '../lib/minikanren_dart.dart';

// Thanks @ ChrisH for providing test cases!

void main() {
  final x = MVar('x');
  final y = MVar('y');
  final z = MVar('z');
  final u = MVar('u');
  final v = MVar('v');
  final w = MVar('w');
  final varCopy = MVar('x');

  test('Var Tests', () {
    expect(isMVar(x), true);
    expect(isMVar(7), false);
    expect(x.isEqual(varCopy), true);
    expect(x == varCopy, true);
    expect(x.isEqual(y), false);
  });

// TODO
  test('Substitution Tests', () {
    expect(isMVar(7), false);
    expect(isMVar(7), false);
  });

  test('Walk Tests', () {
    expect(
        walk(
            z,
            Substitution([
              {MVar('z'): 'a'},
              {MVar('x'): 'w'},
              {MVar('y'): 'z'}
            ])),
        'a');
    expect(
        walk(
            y,
            Substitution([
              {MVar('z'): 'a'},
              {MVar('x'): 'w'},
              {MVar('y'): MVar('z')}
            ])),
        'a');
    expect(
        walk(
            x,
            Substitution([
              {MVar('z'): 'a'},
              {MVar('x'): 'w'},
              {MVar('y'): 'z'}
            ])),
        'w');
    expect(
        walk(
            x,
            Substitution([
              {MVar('z'): 'a'},
              {MVar('x'): MVar('w')},
              {
                MVar('w'): ['x', 'e', 'z']
              }
            ])),
        ['x', 'e', 'z']);
    expect(
        walk(
            w,
            Substitution([
              {MVar('x'): 'b'},
              {MVar('z'): 'y'},
              {
                MVar('w'): ['x', 'e', 'z']
              }
            ])),
        ['x', 'e', 'z']);
    expect(
        walk(
            y,
            Substitution([
              {MVar('y'): x}
            ])),
        x);
  });

  test('Occurs Tests', () {
    expect(
        occurs(
            x,
            y,
            Substitution([
              {y: x}
            ])),
        true);
    expect(
        occurs(
            x,
            'z',
            Substitution([
              {y: x}
            ])),
        false);
    expect(occurs(x, x, Substitution.empty()), true);
    expect(
        occurs(
            x,
            x,
            Substitution([
              {z: 'a'},
              {y: z}
            ])),
        true);
    expect(
        occurs(
            y,
            z,
            Substitution([
              {x: y},
              {w: 'a'},
              {z: x}
            ])),
        true);
    expect(
        occurs(
            y,
            'w',
            Substitution([
              {x: y},
              {w: 'a'},
              {z: x}
            ])),
        false);
    expect(
        occurs(
            y,
            x,
            Substitution([
              {
                x: {MVar('a'): y}
              },
              {z: w}
            ])),
        true);
    expect(
        occurs(
            y,
            {x: 'b'},
            Substitution([
              {
                x: {MVar('a'): y}
              },
              {z: w}
            ])),
        true);
    expect(
        occurs(
            y,
            {'a': 'b'},
            Substitution([
              {
                x: {MVar('a'): y}
              },
              {z: w}
            ])),
        false);
  });

  test('Extend Substitution Tests', () {
    expect(
        ext_s(
            x,
            y,
            Substitution([
              {y: x}
            ])),
        false);
    expect(
        ext_s(
            x,
            z,
            Substitution([
              {y: x}
            ])),
        Substitution([
          {y: x},
          {x: z}
        ]));
    expect(
        ext_s(
            x,
            {z: 'a'},
            Substitution([
              {y: x}
            ])),
        Substitution([
          {y: x},
          {
            x: {z: 'a'}
          }
        ]));
    expect(
        ext_s(
            x,
            {z: y},
            Substitution([
              {y: x}
            ])),
        false);
    expect(
        ext_s(x, 1, Substitution.empty()),
        Substitution([
          {x: 1}
        ]));
    expect(
        ext_s(x, MVar('1'), Substitution.empty()),
        Substitution([
          {x: MVar('1')}
        ]));
  });

  test('Unify Tests', () {
    expect(
        unify(x, 'pie', Substitution.empty()),
        Substitution([
          {x: 'pie'}
        ]));
    expect(unify('pie', 'pot', Substitution.empty()), false);
    expect(
        unify(x, y, Substitution.empty()),
        Substitution([
          {x: y}
        ]));
    expect(unify(x, x, Substitution.empty()), Substitution.empty());
    expect(unify('a', 'a', Substitution.empty()), Substitution.empty());
    expect(
        unify(x, 1, Substitution.empty()),
        Substitution([
          {x: 1}
        ]));
    expect(
        unify(1, x, Substitution.empty()),
        Substitution([
          {x: 1}
        ]));
    expect(unify(1, 2, Substitution.empty()), false);
    expect(unify('a', 'b', Substitution.empty()), false);
    expect(
        unify({x: y}, {'pie': 'pot'}, Substitution.empty()),
        Substitution([
          {x: 'pie'},
          {y: 'pot'}
        ]));
    expect(
        unify({x: y}, {'pie': x}, Substitution.empty()),
        Substitution([
          {x: 'pie'},
          {y: 'pie'}
        ]));
    expect(unify({1: 2}, {2: 1}, Substitution.empty()), false);
    expect(
        unify([
          {x: y},
          {x: y}
        ], [
          {x: y},
          {x: y}
        ], Substitution.empty()),
        Substitution.empty());
    expect(
        unify([
          {1: 2},
          {1: 2}
        ], [
          {1: 2},
          {1: 2}
        ], Substitution.empty()),
        Substitution.empty());
    expect(
        unify(
            Substitution([
              {x: 2},
              {MVar('3'): 4}
            ]),
            Substitution([
              {MVar('1'): 2},
              {MVar('3'): 4}
            ]),
            Substitution.empty()),
        Substitution([
          {x: MVar('1')}
        ]));
    expect(
        unify(
            Substitution([
              {x: 2},
              {MVar('3'): 4}
            ]),
            Substitution([
              {MVar('1'): 9},
              {y: 4}
            ]),
            Substitution.empty()),
        false);
  });

  test('Equals Tests', () {
    expect(mEquals(x, y)(Substitution.empty()), [
      Substitution([
        {x: y}
      ])
    ]);
    expect(s_succeed()(Substitution.empty()), [Substitution.empty()]);
    expect(mEquals(s_succeed(), u_fail())(Substitution.empty()), []);
    expect(mEquals(u_fail(), u_fail())(Substitution.empty()), []);
  });

  test('disj2 Tests', () {
    expect(
        disj2(mEquals(MVar('olive'), MVar('x')),
            mEquals(MVar('oil'), MVar('x')))(empty_s),
        false);
    expect(
        disj2(mEquals(MVar('olive'), MVar('x')),
            mEquals(MVar('oil'), MVar('x')))(empty_s),
        false);
  });

// [['x', 'olive']]
// [['x', 'oil']]

// [['x', 'olive']]

  test('conj2 Tests', () {
    expect(
        conj2(mEquals(MVar('x'), MVar('olive')),
            mEquals(MVar('olive'), MVar('x')))(Substitution([
          {x: MVar('olive')}
        ])),
        false);
    expect(
        conj2(mEquals({2: x}, {2: 3}), mEquals({y: 3}, {2: 3}))(
            Substitution.empty()),
        false);
  });

  //[['x', 'olive']]
//[['y', 2], ['x', 3]]

  test('Take Tests', () {
    final take1 = disj2(mEquals({MVar('olive'): y}, {x: MVar('apple')}),
        mEquals({MVar('oil'): y}, {x: MVar('panda')}))(empty_s);
    expect(take_inf(2, take1), false);
    expect(take_inf(20, take1), false);
  });

//  [[['x', 'olive'], ['y', 'apple']], [['x', 'oil'], ['y', 'panda']]]
// for both

  test('Ifte Tests', () {
    expect(
        ifte(mEquals(x, MVar('butterfly')), mEquals(u, MVar('cat')),
            mEquals(v, MVar('dog')))(Substitution.empty()),
        false);
    expect(
        ifte(mEquals(x, MVar('butterfly')), mEquals(u, MVar('cat')),
            mEquals(v, MVar('dog')))(Substitution([
          {x: MVar('lemon')}
        ])),
        false);
  });

// [['x', 'butterfly'], ['u', 'cat']]
// [['x', 'lemon'], ['v', 'dog']]

  test('Walkstar Tests', () {
    expect(
        walk_star(
            y,
            Substitution([
              {z: 'a'},
              {x: 'w'},
              {y: z}
            ])),
        ['a']);
    expect(
        walk_star(
            w,
            Substitution([
              {x: 'b'},
              {z: y},
              {
                w: ['x', 'e', z]
              }
            ])),
        ['b', 'e', y]);
    expect(
        walk_star(
            w,
            Substitution([
              {
                x: [u, v]
              },
              {z: y},
              {
                w: [x, 'e', z]
              },
              {u: 'c'},
              {v: 'q'}
            ])),
        ['c', 'q', 'e', y]);
  });

  test('Reify Tests', () {
    expect(
        run_goal(
            10,
            reify(disj2(
                mEquals(MVar('olive'), MVar('x')), mEquals(MVar('oil'), x)))),
        false);
  });
}

// [['olive', 'oil'], ['y', 'y'], ['z', 'z'], ['u', 'u'], ['v', 'v'], ['w', 'w']]
