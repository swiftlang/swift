// RUN: %target-swift-frontend -disable-availability-checking -parse-as-library -enable-experimental-feature StrictConcurrency -enable-experimental-feature GlobalConcurrency -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -disable-availability-checking -parse-as-library -enable-experimental-feature StrictConcurrency=complete -enable-experimental-feature GlobalConcurrency -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -disable-availability-checking -parse-as-library -enable-experimental-feature StrictConcurrency=complete -enable-experimental-feature GlobalConcurrency -emit-sil -o /dev/null -verify -enable-experimental-feature RegionBasedIsolation %s

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

@propertyWrapper
public struct TestWrapper {
  public init() {}
  public var wrappedValue: Int {
    return 0
  }
}

struct TestStatics {
  static let immutableExplicitSendable = TestSendable()
  static let immutableNonsendable = TestNonsendable() // expected-warning{{static property 'immutableNonsendable' is not concurrency-safe because it is not either conforming to 'Sendable' or isolated to a global actor}}
  static nonisolated(unsafe) let immutableNonisolatedUnsafe = TestNonsendable()
  static nonisolated let immutableNonisolated = TestNonsendable() // expected-warning{{static property 'immutableNonisolated' is not concurrency-safe because it is not either conforming to 'Sendable' or isolated to a global actor}}
  static let immutableInferredSendable = 0
  static var mutable = 0 // expected-warning{{static property 'mutable' is not concurrency-safe because it is non-isolated global shared mutable state}}
  // expected-note@-1{{isolate 'mutable' to a global actor, or convert it to a 'let' constant and conform it to 'Sendable'}}
  // expected-note@-2{{static property declared here}}
  static var computedProperty: Int { 0 } // computed property that, though static, has no storage so is not a global
  @TestWrapper static var wrapped: Int // expected-warning{{static property 'wrapped' is not concurrency-safe because it is non-isolated global shared mutable state}}
  // expected-note@-1{{isolate 'wrapped' to a global actor, or convert it to a 'let' constant and conform it to 'Sendable'}}
}

@TestGlobalActor
func f() {
  print(TestStatics.immutableExplicitSendable)
  print(TestStatics.immutableInferredSendable)
  print(TestStatics.mutable) // expected-warning{{reference to static property 'mutable' is not concurrency-safe because it involves shared mutable state}}
}
