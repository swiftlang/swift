// RUN: %swift -dump-parse -verify %s

var x:Int

func square(x:Int) -> Int { return x*x }

struct A<B> {
  struct C<D> { }
}

switch x {
// Expressions as patterns.
case 0:
case 1 + 2:
case square(9):

// 'var' pattern.
case var a:
case var var a: // expected-error{{'var' cannot appear nested inside another 'var' pattern}}

// 'Any' pattern.
case _:

// 'is' pattern.
case is A<B>:
case is A<B>.C<D>:
case is (Int, Int):
case is (a:Int, b:Int):

}
