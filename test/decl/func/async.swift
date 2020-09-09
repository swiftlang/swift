// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

// Redeclaration checking
func redecl1() async { } // expected-note{{previously declared here}}
func redecl1() async throws { } // expected-error{{invalid redeclaration of 'redecl1()'}}

// Override checking

class Super {
  func f() async { } // expected-note{{potential overridden instance method 'f()' here}}
  func g() { } // expected-note{{potential overridden instance method 'g()' here}}
  func h() async { }
}

class Sub: Super {
  override func f() { } // expected-error{{method does not override any method from its superclass}}
  override func g() async { } // expected-error{{method does not override any method from its superclass}}
  override func h() async { }
}

// Witness checking
protocol P1 {
  func g() // expected-note{{protocol requires function 'g()' with type '() -> ()'; do you want to add a stub?}}
}

struct ConformsToP1: P1 { // expected-error{{type 'ConformsToP1' does not conform to protocol 'P1'}}
  func g() async { }  // expected-note{{candidate is 'async', but protocol requirement is not}}
}

protocol P2 {
  func f() async // expected-note{{protocol requires function 'f()' with type '() async -> ()'; do you want to add a stub?}}
}

struct ConformsToP2: P2 { // expected-error{{type 'ConformsToP2' does not conform to protocol 'P2'}}
  func f() { }  // expected-note{{candidate is not 'async', but protocol requirement is}}
}
