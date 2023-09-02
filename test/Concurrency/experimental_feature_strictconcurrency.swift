// RUN: %target-swift-frontend -disable-availability-checking -parse-as-library -enable-experimental-feature StrictConcurrency -enable-experimental-feature GlobalConcurrency -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -disable-availability-checking -parse-as-library -enable-experimental-feature StrictConcurrency=complete -enable-experimental-feature GlobalConcurrency -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -disable-availability-checking -parse-as-library -enable-experimental-feature StrictConcurrency=complete -enable-experimental-feature GlobalConcurrency -emit-sil -o /dev/null -verify -enable-experimental-feature SendNonSendable %s

// REQUIRES: concurrency
// REQUIRES: asserts

class C1 { } // expected-note{{class 'C1' does not conform to the 'Sendable' protocol}}
class C2 { }

@available(*, unavailable)
extension C2: Sendable {} // expected-note{{conformance of 'C2' to 'Sendable' has been explicitly marked unavailable here}}

protocol TestProtocol {
  associatedtype Value: Sendable
}

struct Test1: TestProtocol { // expected-warning{{type 'Test1.Value' (aka 'C1') does not conform to the 'Sendable' protocol}}
  typealias Value = C1
}

struct Test2: TestProtocol { // expected-warning{{conformance of 'C2' to 'Sendable' is unavailable}}
  // expected-note@-1{{in associated type 'Self.Value' (inferred as 'C2')}}
  typealias Value = C2
}

@globalActor
actor TestGlobalActor {
  static var shared = TestGlobalActor()
}

@TestGlobalActor
var mutableIsolatedGlobal = 1

var mutableNonisolatedGlobal = 1 // expected-warning{{var 'mutableNonisolatedGlobal' is not concurrency-safe because it is non-isolated global shared mutable state}}
// expected-note@-1{{isolate 'mutableNonisolatedGlobal' to a global actor, or convert it to a 'let' constant and conform it to 'Sendable'}}

let immutableGlobal = 1

final class TestSendable: Sendable {
  init() {}
}

final class TestNonsendable {
  init() {}
}

struct A {
  static let immutableExplicitSendable = TestSendable()
  static let immutableNonsendableGlobal = TestNonsendable() // expected-warning{{static property 'immutableNonsendableGlobal' is not concurrency-safe because it is not either conforming to 'Sendable' or isolated to a global actor}}
  static let immutableInferredSendable = 0
  static var mutable = 0 // expected-warning{{static property 'mutable' is not concurrency-safe because it is non-isolated global shared mutable state}}
  // expected-note@-1{{isolate 'mutable' to a global actor, or convert it to a 'let' constant and conform it to 'Sendable'}}
  // expected-note@-2{{static property declared here}}
}

@TestGlobalActor
func f() {
  print(A.immutableExplicitSendable)
  print(A.immutableInferredSendable)
  print(A.mutable) // expected-warning{{reference to static property 'mutable' is not concurrency-safe because it involves shared mutable state}}
}
