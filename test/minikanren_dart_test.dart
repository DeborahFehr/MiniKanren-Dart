import 'package:minikanren_dart/minikanren_dart.dart';
import 'package:minikanren_dart/mk_interface.dart';
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

// TODO ?
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
                MVar('w'): ['x', 'e', MVar('z')]
              }
            ])),
        ['x', 'e', z]);
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
    expect(mEquals(x, 'olive')(Substitution.empty()), [
      Substitution([
        {x: 'olive'}
      ])
    ]);
    expect(
        mEquals(x, 'olive')(Substitution([
          {x: 'oil'}
        ])),
        []);
    expect(s_succeed()(Substitution.empty()), [Substitution.empty()]);
    expect(mEquals(s_succeed(), u_fail())(Substitution.empty()), []);
    expect(mEquals(u_fail(), u_fail())(Substitution.empty()), []);
  });

  test('disj2 Tests', () {
    expect(
        disj2(mEquals('olive', x), mEquals('oil', x))(Substitution.empty()), [
      Substitution([
        {x: 'olive'}
      ]),
      Substitution([
        {x: 'oil'}
      ])
    ]);
    expect(
        disj2(mEquals('olive', x), mEquals('oil', x))(Substitution([
          {x: 'olive'}
        ])),
        [
          Substitution([
            {x: 'olive'}
          ])
        ]);
  });

  test('conj2 Tests', () {
    expect(
        conj2(mEquals(x, 'olive'), mEquals('olive', x))(Substitution([
          {x: 'olive'}
        ])),
        [
          Substitution([
            {x: 'olive'}
          ])
        ]);

    expect(
        conj2(mEquals({2: x}, {2: 3}), mEquals({y: 3}, {2: 3}))(
            Substitution.empty()),
        [
          Substitution([
            {x: 3},
            {y: 2}
          ])
        ]);
  });

  test('Walkstar Tests', () {
    expect(
        walk_star(
            y,
            Substitution([
              {z: 'a'},
              {x: 'w'},
              {y: z}
            ])),
        'a');
    expect(
        walk_star(
            w,
            Substitution([
              {x: 'b'},
              {z: y},
              {
                w: [x, 'e', z]
              }
            ])),
        ['b', 'e', y]);
    expect(
        walk_star(
            w,
            Substitution([
              {x: u},
              {z: y},
              {
                w: [x, 'e', z]
              },
              {u: 'c'}
            ])),
        ['c', 'e', y]);
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
        [
          ['c', 'q'],
          'e',
          y
        ]);
  });

  test('Take Tests', () {
    final take1 = disj2(mEquals({'olive': y}, {x: 'apple'}),
        mEquals({'oil': y}, {x: 'panda'}))(Substitution.empty());
    final takeResult = [
      Substitution([
        {x: 'olive'},
        {y: 'apple'}
      ]),
      Substitution([
        {x: 'oil'},
        {y: 'panda'}
      ])
    ];
    expect(take_inf(2, take1), takeResult);
    expect(take_inf(20, take1), takeResult);
  });

  test('Reify Tests', () {
    final reify1 = run_goal(5, disj2(mEquals('olive', x), mEquals('oil', x)));
    expect(reify(x)(reify1[0]), 'olive');
    expect(reify(x)(reify1[1]), 'oil');
    expect(
        reify(x)(Substitution([
          {x: y}
        ])),
        '_0');
  });

  test('Ifte Tests', () {
    expect(
        ifte(mEquals(x, 'butterfly'), mEquals(u, 'cat'), mEquals(v, 'dog'))(
            Substitution.empty()),
        [
          Substitution([
            {x: 'butterfly'},
            {u: 'cat'}
          ])
        ]);
    expect(
        ifte(mEquals(x, 'butterfly'), mEquals(u, 'cat'), mEquals(v, 'dog'))(
            Substitution([
          {x: 'lemon'}
        ])),
        [
          Substitution([
            {x: 'lemon'},
            {v: 'dog'}
          ])
        ]);
  });

  test('Disj Tests', () {
    expect(
        disj([mEquals('olive', x), mEquals('oil', x)])(Substitution.empty()), [
      Substitution([
        {x: 'olive'}
      ]),
      Substitution([
        {x: 'oil'}
      ]),
    ]);
    expect(
        disj([mEquals('olive', x), mEquals('oil', x), mEquals('pan', x)])(
            Substitution.empty()),
        [
          Substitution([
            {x: 'olive'}
          ]),
          Substitution([
            {x: 'oil'}
          ]),
          Substitution([
            {x: 'pan'}
          ]),
        ]);
    expect(disj([]), u_fail);
  });

  test('Conj Tests', () {
    expect(
        conj([
          mEquals({2: x}, {2: 3}),
          mEquals({y: 3}, {2: 3})
        ])(Substitution.empty()),
        [
          Substitution([
            {x: 3},
            {y: 2}
          ])
        ]);
    expect(
        conj([
          mEquals({2: x}, {2: 3}),
          mEquals({y: 3}, {2: 3}),
          mEquals({z: 4}, {1: 4})
        ])(Substitution.empty()),
        [
          Substitution([
            {x: 3},
            {y: 2},
            {z: 1}
          ])
        ]);
    expect(conj([]), s_succeed);
  });
  test('Run_Star Tests', () {
    expect(run_star('q', [u_fail()]), []);
    expect(run_star('q', [s_succeed()]), ['_0']);
    expect(run_star('x', [mEquals(x, x)]), ['_0']);
    expect(run_star('x', [mEquals([], [])]), ['_0']);
    expect(run_star('x', [mEquals(x, 'pea')]), ['pea']);
    expect(run_star('x', [disj2(mEquals(x, 'olive'), mEquals(x, 'oil'))]),
        ['olive', 'oil']);
    expect(
        run_star('x',
            [disj2(conj2(mEquals(x, 'olive'), u_fail()), mEquals(x, 'oil'))]),
        ['oil']);
  });

  test('Conde Tests', () {
    expect(
        run_star('x', [
          condE([mEquals(x, 'olive'), mEquals(y, 'oil')])
        ]),
        ['olive']);
    expect(
        run_star('y', [
          condE([mEquals(x, 'olive'), mEquals(y, 'oil')])
        ]),
        ['oil']);
    expect(
        run_star('x', [
          condE([
            mEquals(x, 'olive'),
            mEquals(y, 'oil'),
            mEquals(x, 'pea'),
            mEquals(y, 'pan')
          ])
        ]),
        ['olive', 'pea']);
  });

  test('Defrel Tests', () {
    expect(run_star('q', [u_fail()]), []);
  });
}
