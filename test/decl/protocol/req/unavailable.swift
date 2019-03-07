// RUN: %target-typecheck-verify-swift -swift-version 4 -enable-objc-interop

// An @objc protocol can have 'unavailable'
// methods.  They are treated as if they
// were marked optional
@objc protocol Proto {
  @objc @available(*,unavailable) optional func bad()
  func good()
}

class Foo : Proto {
  @objc func good() {}
}

// Reject protocols with 'unavailable' requirements
// if a protocol is not marked @objc.
protocol NonObjCProto {
  @available(*,unavailable) func bad() // expected-error {{protocol members can only be marked unavailable in an @objc protocol}} expected-note {{protocol requires function 'bad()'}}
  func good()
}

class Bar : NonObjCProto { // expected-error {{type 'Bar' does not conform to protocol 'NonObjCProto'}}
  func good() {}
}


// Complain about unavailable witnesses (error in Swift 4, warning in Swift 3)
protocol P {
  func foo(bar: Foo) // expected-note 2 {{requirement 'foo(bar:)' declared here}}
}

struct ConformsToP : P { // expected-error{{type 'ConformsToP' does not conform to protocol 'P'}}
  @available(*, unavailable)
  func foo(bar: Foo) { } // expected-error{{unavailable instance method 'foo(bar:)' was used to satisfy a requirement of protocol 'P'}}
}

struct ConformsToP2 {
  @available(*, unavailable)
  func foo(bar: Foo) { } // expected-note {{'foo(bar:)' declared here}}
}
extension ConformsToP2: P {} // expected-error{{type 'ConformsToP2' does not conform to protocol 'P'}}
// expected-error@-1 {{unavailable instance method 'foo(bar:)' was used to satisfy a requirement of protocol 'P'}}
