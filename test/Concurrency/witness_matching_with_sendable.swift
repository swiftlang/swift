// RUN: %target-typecheck-verify-swift -swift-version 5 -verify-additional-prefix swift5-
// RUN: %target-typecheck-verify-swift -swift-version 5 -strict-concurrency=complete -verify-additional-prefix swift6-
// RUN: %target-typecheck-verify-swift -swift-version 6 -verify-additional-prefix swift6-

protocol P {
  @preconcurrency var prop: [String: any Sendable] { get set }
  // expected-swift5-note@-1 3 {{expected sendability to match requirement here}}
  // expected-swift6-note@-2 3 {{protocol requires property 'prop' with type '[String : any Sendable]'}}
  @preconcurrency func reqFn(_: [String: any Sendable], _: @Sendable ((any Sendable)?) -> Void)
  // expected-swift5-note@-1 2 {{expected sendability to match requirement here}}
  // expected-swift6-note@-2 2 {{protocol requires function 'reqFn' with type '([String : any Sendable], @Sendable ((any Sendable)?) -> Void) -> ()'}}
}

struct S1 : P { // expected-swift6-error {{type 'S1' does not conform to protocol 'P'}} expected-swift6-note {{add stubs for conformance}}
  var prop: [String: Any] = [:]
  // expected-swift5-warning@-1 {{sendability of function types in property 'prop' does not match requirement in protocol 'P'; this is an error in the Swift 6 language mode}}
  // expected-swift6-note@-2 {{candidate has non-matching type '[String : Any]'}}
  func reqFn(_: [String: Any], _: (Any?) -> Void) {}
  // expected-swift5-warning@-1 {{sendability of function types in instance method 'reqFn' does not match requirement in protocol 'P'; this is an error in the Swift 6 language mode}}
  // expected-swift6-note@-2 {{candidate has non-matching type '([String : Any], (Any?) -> Void) -> ()'}}
}

struct S2 : P { // expected-swift6-error {{type 'S2' does not conform to protocol 'P'}} expected-swift6-note {{add stubs for conformance}}
  var prop: [String: Any] = [:]
  // expected-swift5-warning@-1 {{sendability of function types in property 'prop' does not match requirement in protocol 'P'; this is an error in the Swift 6 language mode}}
  // expected-swift6-note@-2 {{candidate has non-matching type '[String : Any]'}}

  func reqFn(_: [String: Any], _: ((any Sendable)?) -> Void) {}
  // expected-swift5-warning@-1 {{sendability of function types in instance method 'reqFn' does not match requirement in protocol 'P'; this is an error in the Swift 6 language mode}}
  // expected-swift6-note@-2 {{candidate has non-matching type '([String : Any], ((any Sendable)?) -> Void) -> ()'}}
}

struct S3 : P { // expected-swift6-error {{type 'S3' does not conform to protocol 'P'}} expected-swift6-note {{add stubs for conformance}}
  var prop: [String: Any] = [:]
  // expected-swift5-warning@-1 {{sendability of function types in property 'prop' does not match requirement in protocol 'P'; this is an error in the Swift 6 language mode}}
  // expected-swift6-note@-2 {{candidate has non-matching type '[String : Any]'}}

  func reqFn(_: [String: any Sendable], _: ((any Sendable)?) -> Void) {} // Ok
}

protocol Q {
  var prop: [String: [String: any Sendable]] { get set }
  // expected-note@-1 {{protocol requires property 'prop' with type '[String : [String : any Sendable]]'}}

  func test(_: [() -> (any Sendable)?])
  // expected-note@-1 {{protocol requires function 'test' with type '([() -> (any Sendable)?]) -> ()'}}
}

struct S4 : Q { // expected-error {{type 'S4' does not conform to protocol 'Q'}} expected-note {{add stubs for conformance}}
  var prop: [String: [String: Any]] = [:]
  // expected-note@-1 {{candidate has non-matching type '[String : [String : Any]]'}}

  func test(_: [() -> Any?]) {}
  // expected-note@-1 {{candidate has non-matching type '([() -> Any?]) -> ()'}}
}

// Test that client is allowed to adopt concurrency first.
protocol PreConcurrency {
  var prop: [String: Any] { get } // expected-swift6-note {{protocol requires property 'prop' with type '[String : Any]'}}
  func req(_: [String: Any], _: ((Any)?) -> Void) // expected-swift6-note {{protocol requires function 'req' with type '([String : Any], (Any?) -> Void) -> ()'}}
}

class Adopter {
  @preconcurrency var prop: [String: any Sendable] = [:] // expected-swift6-note {{candidate has non-matching type '[String : any Sendable]'}}
}

extension Adopter : PreConcurrency { // expected-swift6-error {{type 'Adopter' does not conform to protocol 'PreConcurrency'}} expected-swift6-note {{add stubs for conformance}}
  @preconcurrency func req(_: [String: any Sendable], _: ((any Sendable)?) -> Void) {}
  // expected-swift6-note@-1 {{candidate has non-matching type '([String : any Sendable], ((any Sendable)?) -> Void) -> ()'}}
}
