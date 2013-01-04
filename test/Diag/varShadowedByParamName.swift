// RUN: %swift %s -i | FileCheck %s
// XFAIL: *
func foo(x : Int) {
}

func test() {
  var x : Int
  var y : Int = 42
  foo(x = y)  // expected-warning{{the variable 'x' is shadowed by the function parameter of the same name}}
}
test()
