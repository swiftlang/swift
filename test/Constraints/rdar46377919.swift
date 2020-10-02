// RUN: %target-typecheck-verify-swift

class Foo {
  init(lhs: @autoclosure () -> Int,
       rhs: @autoclosure () -> Undefined) {
     // expected-error@-1 {{cannot find type 'Undefined' in scope}}
  }
}

func foo() -> Foo {
  return Foo(lhs: 2, rhs: 2)
}
