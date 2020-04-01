// RUN: %target-typecheck-verify-swift

class Foo {
  init(lhs: @autoclosure () -> Int,
       rhs: @autoclosure () -> Undefined) {
     // expected-error@-1 {{use of undeclared type 'Undefined'}}
  }
}

func foo() -> Foo {
  return Foo(lhs: 2, rhs: 2)
}
