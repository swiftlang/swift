// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 %s

protocol P {}

struct Foo {
  mutating func boom() {}
}

let x = Foo.boom // expected-error{{cannot reference 'mutating' method as function value}}
var y = Foo()
let z0 = y.boom // expected-error{{cannot reference 'mutating' method as function value}}
let z1 = Foo.boom(&y) // expected-error{{cannot reference 'mutating' method as function value}}

func fromLocalContext() -> (inout Foo) -> () -> () {
  return Foo.boom // expected-error{{cannot reference 'mutating' method as function value}}
}
func fromLocalContext2(x: inout Foo, y: Bool) -> () -> () {
  if y {
    return x.boom // expected-error{{cannot reference 'mutating' method as function value}}
  } else {
    return Foo.boom(&x) // expected-error{{cannot reference 'mutating' method as function value}}
  }
}

func bar() -> P.Type { fatalError() }
func bar() -> Foo.Type { fatalError() }

_ = bar().boom       // expected-error{{cannot reference 'mutating' method as function value}}
_ = bar().boom(&y)   // expected-error{{cannot reference 'mutating' method as function value}}
_ = bar().boom(&y)() // expected-error{{cannot reference 'mutating' method as function value}}

func foo(_ foo: Foo.Type) {
  _ = foo.boom       // expected-error{{cannot reference 'mutating' method as function value}}
  _ = foo.boom(&y)   // expected-error{{cannot reference 'mutating' method as function value}}
  _ = foo.boom(&y)() // expected-error{{cannot reference 'mutating' method as function value}}
}
