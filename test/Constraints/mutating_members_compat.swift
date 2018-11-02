// RUN: %target-swift-frontend -typecheck -verify -swift-version 4 %s

struct Foo {
  mutating func boom() {}
}

let x = Foo.boom // expected-warning{{partial application of 'mutating' method}}
var y = Foo()
let z0 = y.boom // expected-error{{partial application of 'mutating' method}}
let z1 = Foo.boom(&y) // expected-error{{partial application of 'mutating' method}}

func fromLocalContext() -> (inout Foo) -> () -> () {
  return Foo.boom // expected-warning{{partial application of 'mutating' method}}
}
func fromLocalContext2(x: inout Foo, y: Bool) -> () -> () {
  if y {
    return x.boom // expected-error{{partial application of 'mutating' method}}
  } else {
    return Foo.boom(&x) // expected-error{{partial application of 'mutating' method}}
  }
}

