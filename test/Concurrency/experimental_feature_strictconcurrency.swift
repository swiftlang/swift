// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/GlobalVariables.swiftmodule -module-name GlobalVariables -parse-as-library -strict-concurrency=minimal -swift-version 5 %S/Inputs/GlobalVariables.swift
// RUN: %target-swift-frontend -disable-availability-checking -parse-as-library -enable-experimental-feature StrictConcurrency -enable-experimental-feature GlobalConcurrency -swift-version 6 -I %t %s %s -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -disable-availability-checking -parse-as-library -enable-experimental-feature StrictConcurrency=complete -enable-experimental-feature GlobalConcurrency -swift-version 6 -I %t %s %s -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -disable-availability-checking -parse-as-library -enable-experimental-feature StrictConcurrency=complete -enable-experimental-feature GlobalConcurrency -swift-version 6 -I %t %s %s -emit-sil -o /dev/null -verify -verify-additional-prefix region-isolation- -enable-experimental-feature RegionBasedIsolation %s

// REQUIRES: concurrency
// REQUIRES: asserts

@preconcurrency import GlobalVariables

class C1 { } // expected-note{{class 'C1' does not conform to the 'Sendable' protocol}}
class C2 { }

@available(*, unavailable)
extension C2: Sendable {} // expected-note{{conformance of 'C2' to 'Sendable' has been explicitly marked unavailable here}}

protocol TestProtocol {
  associatedtype Value: Sendable
}

struct Test1: TestProtocol { // expected-error{{type 'Test1.Value' (aka 'C1') does not conform to the 'Sendable' protocol}}
  typealias Value = C1
}

struct Test2: TestProtocol { // expected-error{{conformance of 'C2' to 'Sendable' is unavailable}}
  // expected-note@-1{{in associated type 'Self.Value' (inferred as 'C2')}}
  typealias Value = C2
}

@globalActor
actor TestGlobalActor {
  static var shared = TestGlobalActor()
}

@TestGlobalActor
var mutableIsolatedGlobal = 1

var mutableNonisolatedGlobal = 1 // expected-error{{var 'mutableNonisolatedGlobal' is not concurrency-safe because it is non-isolated global shared mutable state}}
// expected-note@-1{{isolate 'mutableNonisolatedGlobal' to a global actor, or convert it to a 'let' constant and conform it to 'Sendable'}}

let immutableGlobal = 1

final class TestSendable: Sendable {
  init() {}
}

final class TestNonsendable {
  init() {}
}

nonisolated(unsafe) let immutableNonisolatedUnsafeTopLevelGlobal = TestNonsendable()

@propertyWrapper
public struct TestWrapper {
  public init() {}
  public var wrappedValue: Int {
    return 0
  }
}

struct TestStatics {
  static let immutableExplicitSendable = TestSendable()
  static let immutableNonsendable = TestNonsendable() // expected-error{{static property 'immutableNonsendable' is not concurrency-safe because it is not either conforming to 'Sendable' or isolated to a global actor}}
  static nonisolated(unsafe) let immutableNonisolatedUnsafe = TestNonsendable()
  static nonisolated let immutableNonisolated = TestNonsendable() // expected-error{{static property 'immutableNonisolated' is not concurrency-safe because it is not either conforming to 'Sendable' or isolated to a global actor}}
  static let immutableInferredSendable = 0
  static var mutable = 0 // expected-error{{static property 'mutable' is not concurrency-safe because it is non-isolated global shared mutable state}}
  // expected-note@-1{{isolate 'mutable' to a global actor, or convert it to a 'let' constant and conform it to 'Sendable'}}
  // expected-note@-2{{static property declared here}}
  static var computedProperty: Int { 0 } // computed property that, though static, has no storage so is not a global
  @TestWrapper static var wrapped: Int // expected-error{{static property 'wrapped' is not concurrency-safe because it is non-isolated global shared mutable state}}
  // expected-note@-1{{isolate 'wrapped' to a global actor, or convert it to a 'let' constant and conform it to 'Sendable'}}
}

@TestGlobalActor
func f() {
  print(TestStatics.immutableExplicitSendable)
  print(TestStatics.immutableInferredSendable)
  print(TestStatics.mutable) // expected-error{{reference to static property 'mutable' is not concurrency-safe because it involves shared mutable state}}
  print(Globals.actorInteger) // expected-error{{main actor-isolated static property 'actorInteger' can not be referenced from global actor 'TestGlobalActor'}}
}

func testLocalNonisolatedUnsafe() async {
  nonisolated(unsafe) var value = 1
  let task = Task {
    value = 2
    return value
  }
  print(await task.value)
}

func testCGlobals() { // expected-note{{add '@MainActor' to make global function 'testCGlobals()' part of global actor 'MainActor'}}
  let _ = Globals.integerConstant
  let _ = Globals.integerMutable // expected-warning{{reference to static property 'integerMutable' is not concurrency-safe because it involves shared mutable state}}
  let _ = Globals.nonisolatedUnsafeIntegerConstant
  let _ = Globals.nonisolatedUnsafeIntegerMutable
  let _ = Globals.actorInteger // expected-error{{main actor-isolated static property 'actorInteger' can not be referenced from a non-isolated context}}
}

@MainActor
func iterate(stream: AsyncStream<Int>) async {
  nonisolated(unsafe) var it = stream.makeAsyncIterator()
  // FIXME: Region isolation should consider a value from a 'nonisolated(unsafe)'
  // declaration to be in a disconnected region

  // expected-region-isolation-warning@+2 {{passing argument of non-sendable type 'AsyncStream<Int>.Iterator' from main actor-isolated context to nonisolated context at this call site could yield a race with accesses later in this function}}
  // expected-region-isolation-note@+1 {{access here could race}}
  while let element = await it.next() {
    print(element)
  }

  // expected-region-isolation-warning@+2 {{passing argument of non-sendable type 'AsyncStream<Int>.Iterator' from main actor-isolated context to nonisolated context at this call site could yield a race with accesses later in this function}}
  // expected-region-isolation-note@+1 {{access here could race}}
  for await x in stream {
    print(x)
  }
}
