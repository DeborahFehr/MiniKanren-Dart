# MiniKanren-Dart
Implementation of miniKanren in Dart for the MiniKanren Seminar

###### Install Dart
To use the implementation you need to install Dart. 
[Here are the instructions](https://dart.dev/tutorials/server/get-started) to install Dart based on your OS.
Then run `dart pub get` to get the necessary dependencies.
To run the main file use `dart run` in the top directory.

###### Example
Examples can be found in the main file. This is the implementation of the teacupo function based on frame 1:82 of the Reasoned Schemer.

    Function teacupo(dynamic t) => disj([mEquals('tea', t), mEquals('cup', t)]);

    print(run_star('t', [teacupo(MVar('t'))])); // returns [tea, cup]
    print(run_star('t', [teacupo('tea')])); // returns [_0]
    print(run_star('t', [teacupo('pie')])); // returns []