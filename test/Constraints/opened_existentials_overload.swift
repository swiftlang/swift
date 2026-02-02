// RUN: %target-typecheck-verify-swift

protocol P {}

func g(_: some P) {}
// expected-note@-1 2{{required by global function 'g' where 'some P' = 'any P'}}

// rdar://problem/160389221
func good(_ x: Array<any P>) {
  Array(x).forEach { y in g(y) }
}

extension Array {
  var ffirst: Element? { fatalError() }
  func ffirst(wwhere: (Element) -> Bool) -> Element { fatalError() }
}

func bad(_ x: Array<any P>) {
  let y = x.ffirst!
  g(y) // ok

  let yy = x.ffirst
  g(yy!) // ok

  // FIXME: This is broken

  g(x.ffirst!)
  // expected-error@-1 {{type 'any P' cannot conform to 'P'}}
  // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
}

func testSubscript() {
  struct S1<T> {
    subscript() -> T { fatalError() }
  }
  func foo(_ xs: S1<any P>) {
    g(xs[])
  }

  struct S2<T> {
    subscript() -> Int { fatalError() }
    subscript() -> T { fatalError() }
  }
  func foo(_ xs: S2<any P>) {
    // FIXME: This should work, if you fix this you can also remove the
    // dynamic member lookup hack in `addSubscriptConstraints`, we should always
    // just add the applicable fn as an unsolved constraint before the member.
    g(xs[])
    // expected-error@-1 {{type 'any P' cannot conform to 'P'}}
    // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
  }
}
