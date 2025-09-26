// RUN: %target-typecheck-verify-swift

protocol P {}

func g(_: some P) {}
// expected-note@-1 {{required by global function 'g' where 'some P' = 'any P'}}

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
