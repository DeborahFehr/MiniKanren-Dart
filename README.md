# MiniKanren-Dart
Implementation of miniKanren in Dart for the [Relational Programming with miniKanren Seminar](http://ps.informatik.uni-tuebingen.de/teaching/ws20/miniKanren/)

### Install Dart
To use the implementation you need to install Dart. 
[Here are the instructions](https://dart.dev/tutorials/server/get-started) to install Dart based on your OS.
Then run `dart pub get` to get the necessary dependencies.
To run the main file use `dart run` in the top directory. This runs the file in the bin folder.

### Example
Examples can be found in the main file. This is the implementation of the teacupo function based on frame 1:82 of [The Reasoned Schemer](https://mitpress.mit.edu/books/reasoned-schemer-second-edition).

    Function teacupo(dynamic t) => disj([mEquals('tea', t), mEquals('cup', t)]);

    print(run_star('t', [teacupo(MVar('t'))])); // returns [tea, cup]
    print(run_star('t', [teacupo('tea')])); // returns [_0]
    print(run_star('t', [teacupo('pie')])); // returns []

More details on how to run functions can be found in the test folder which provides test cases for multiple functions.