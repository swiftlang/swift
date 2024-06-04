// RUN: %target-swift-frontend -swift-version 6 -verify -c %s

// READ THIS! This file only contains tests that validate that the relevant
// function subtyping rules for sending work. Please do not put other tests in
// the file!

// REQUIRES: concurrency
// REQUIRES: asserts

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {}

protocol ProtocolWithSendingReqs {
  func sendingResult() -> sending NonSendableKlass // expected-note {{}}
  func nonSendingParam(_ x: NonSendableKlass) // expected-note {{}}
}

protocol ProtocolWithMixedReqs {
  func nonSendingParamAndSendingResult(_ x: NonSendableKlass) -> sending NonSendableKlass // expected-note 4{{}}
}

/////////////////
// MARK: Tests //
/////////////////

struct MatchSuccess : ProtocolWithSendingReqs, ProtocolWithMixedReqs {
  func sendingResult() -> sending NonSendableKlass { fatalError() }
  func nonSendingParam(_ x: NonSendableKlass) -> () { fatalError() }
  func nonSendingParamAndSendingResult(_ x: NonSendableKlass) -> sending NonSendableKlass { fatalError() }
}

struct FailToMatch : ProtocolWithSendingReqs, ProtocolWithMixedReqs { // expected-error 2{{}}
  func sendingResult() -> NonSendableKlass { fatalError() }
  // expected-note @-1 {{candidate has non-matching type '() -> NonSendableKlass'}}
  func nonSendingParam(_ x: sending NonSendableKlass) -> () { fatalError() }
  // expected-note @-1 {{candidate has non-matching type '(sending NonSendableKlass) -> ()'}}
  func nonSendingParamAndSendingResult(_ x: sending NonSendableKlass) -> NonSendableKlass { fatalError() }
  // expected-note @-1 {{candidate has non-matching type '(sending NonSendableKlass) -> NonSendableKlass'}}
}

struct FailToMatch2 : ProtocolWithMixedReqs { // expected-error {{}}
  func nonSendingParamAndSendingResult(_ x: sending NonSendableKlass) -> NonSendableKlass { fatalError() }
  // expected-note @-1 {{candidate has non-matching type '(sending NonSendableKlass) -> NonSendableKlass'}}
}

struct FailToMatch3 : ProtocolWithMixedReqs { // expected-error {{}}
  func nonSendingParamAndSendingResult(_ x: NonSendableKlass) -> NonSendableKlass { fatalError() }
  // expected-note @-1 {{candidate has non-matching type '(NonSendableKlass) -> NonSendableKlass'}}
}

struct FailToMatch4 : ProtocolWithMixedReqs { // expected-error {{}}
  func nonSendingParamAndSendingResult(_ x: sending NonSendableKlass) -> sending NonSendableKlass { fatalError() }
  // expected-note @-1 {{candidate has non-matching type '(sending NonSendableKlass) -> sending NonSendableKlass'}}
}
