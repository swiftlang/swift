// RUN: %target-swift-frontend -typecheck -verify -swift-version 4 %s

protocol P {}

struct Foo {
  mutating func boom() {}
}

let x = Foo.boom // expected-warning{{cannot reference 'mutating' method as function value; calling the function has undefined behavior and will be an error in future Swift versions}}
var y = Foo()
let z0 = y.boom // expected-error{{cannot reference 'mutating' method as function value}}
let z1 = Foo.boom(&y) // expected-error{{cannot reference 'mutating' method as function value}}

func fromLocalContext() -> (inout Foo) -> () -> () {
  return Foo.boom // expected-warning{{cannot reference 'mutating' method as function value; calling the function has undefined behavior and will be an error in future Swift versions}}
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

_ = bar().boom       // expected-warning{{cannot reference 'mutating' method as function value; calling the function has undefined behavior and will be an error in future Swift versions}}
_ = bar().boom(&y)   // expected-error{{cannot reference 'mutating' method as function value}}
_ = bar().boom(&y)() // expected-error{{cannot reference 'mutating' method as function value}}

func foo(_ foo: Foo.Type) {
  _ = foo.boom       // expected-warning{{cannot reference 'mutating' method as function value; calling the function has undefined behavior and will be an error in future Swift versions}}
  _ = foo.boom(&y)   // expected-error{{cannot reference 'mutating' method as function value}}
  _ = foo.boom(&y)() // expected-error{{cannot reference 'mutating' method as function value}}
}
