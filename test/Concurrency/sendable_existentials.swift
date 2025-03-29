// RUN: %target-swift-frontend -strict-concurrency=targeted -emit-sil -o /dev/null %s -verify
// RUN: %target-swift-frontend -strict-concurrency=complete -verify-additional-prefix complete-and-tns- -verify-additional-prefix complete- -emit-sil -o /dev/null %s -verify
// RUN: %target-swift-frontend -strict-concurrency=complete -verify-additional-prefix complete-and-tns- -emit-sil -o /dev/null %s -verify -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_RegionBasedIsolation

@preconcurrency func send(_: Sendable) { }
func sendOpt(_: Sendable?) { }

enum E {
  case something(Sendable)
}

@available(SwiftStdlib 5.1, *)
func testE(a: Any, aOpt: Any?) async {
  send(a) // expected-warning{{type 'Any' does not conform to the 'Sendable' protocol}}
  sendOpt(a) // expected-warning{{type 'Any' does not conform to the 'Sendable' protocol}}
  sendOpt(aOpt) // expected-warning{{type 'Any' does not conform to the 'Sendable' protocol}}

  let _: E = .something(a) // expected-warning{{type 'Any' does not conform to the 'Sendable' protocol}}
  _ = E.something(a) // expected-warning{{type 'Any' does not conform to the 'Sendable' protocol}}

  var sendable: Sendable
  sendable = a // expected-warning{{type 'Any' does not conform to the 'Sendable' protocol}}

  var arrayOfSendable: [Sendable]
  arrayOfSendable = [a, a] // expected-warning 2{{type 'Any' does not conform to the 'Sendable' protocol}}

  func localFunc() { }
  sendable = localFunc // expected-warning{{type '() -> ()' does not conform to the 'Sendable' protocol}}
  // expected-note@-1{{a function type must be marked '@Sendable' to conform to 'Sendable'}}

  _ = sendable
  _ = arrayOfSendable
}

func testESilently(a: Any, aOpt: Any?) {
  send(a) // expected-complete-and-tns-warning {{'Any' does not conform to the 'Sendable' protocol}}
  sendOpt(a) // expected-complete-and-tns-warning {{'Any' does not conform to the 'Sendable' protocol}}
  sendOpt(aOpt) // expected-complete-and-tns-warning {{'Any' does not conform to the 'Sendable' protocol}}

  let _: E = .something(a) // expected-complete-and-tns-warning {{'Any' does not conform to the 'Sendable' protocol}}
  _ = E.something(a) // expected-complete-and-tns-warning {{'Any' does not conform to the 'Sendable' protocol}}

  var sendable: Sendable
  sendable = a // expected-complete-and-tns-warning {{'Any' does not conform to the 'Sendable' protocol}}

  var arrayOfSendable: [Sendable]
  arrayOfSendable = [a, a] // expected-complete-and-tns-warning 2{{'Any' does not conform to the 'Sendable' protocol}}

  func localFunc() { }
  sendable = localFunc // expected-complete-and-tns-warning {{'() -> ()' does not conform to the 'Sendable' protocol}}
  // expected-complete-and-tns-note @-1 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  _ = sendable
  _ = arrayOfSendable
}

func testErasure() {
  class A {}
  class B : A {}

  func produce() -> any B & Sendable {
    fatalError()
  }

  let _: any A & Sendable = produce() // no warning
}
