// RUN: %target-typecheck-verify-swift -strict-concurrency=targeted
// REQUIRES: concurrency
// REQUIRES: OS=macosx

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
  send(a)
  sendOpt(a)
  sendOpt(aOpt)

  let _: E = .something(a)
  _ = E.something(a)

  var sendable: Sendable
  sendable = a

  var arrayOfSendable: [Sendable]
  arrayOfSendable = [a, a]

  func localFunc() { }
  sendable = localFunc

  _ = sendable
  _ = arrayOfSendable
}
