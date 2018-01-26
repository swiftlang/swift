// RUN: %target-swift-frontend -typecheck -verify %s

// SR-5163
func sr5163() {
  func foo(_ x: Int) -> Int? { return 1 }
  
  func fn() {
    let a = foo(c) // expected-error {{use of local variable 'c' before its declaration}}
    guard let b = a else { return }
    let c = b // expected-note {{'c' declared here}}
  }
}

// SR-6726
var foo: Int?

func test() {
  guard let bar = foo else { // expected-error {{use of local variable 'foo' before its declaration}}
    return
  }
  let foo = String(bar) // expected-note {{'foo' declared here}}
}
