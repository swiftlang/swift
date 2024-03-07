// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/GlobalVariables.swiftmodule -module-name GlobalVariables -parse-as-library -strict-concurrency=minimal -swift-version 5 %S/Inputs/GlobalVariables.swift
// RUN: %target-swift-frontend -disable-availability-checking -parse-as-library -swift-version 6 -I %t -emit-sil -o /dev/null -verify %s

// REQUIRES: concurrency

@preconcurrency import GlobalVariables

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
  static let immutableNonsendable = TestNonsendable() // expected-error{{static property 'immutableNonsendable' is not concurrency-safe because non-'Sendable' type 'TestNonsendable' may have shared mutable state}}
  // expected-note@-1 {{isolate 'immutableNonsendable' to a global actor, or conform 'TestNonsendable' to 'Sendable'}}
  static nonisolated(unsafe) let immutableNonisolatedUnsafe = TestNonsendable()
  static nonisolated let immutableNonisolated = TestNonsendable() // expected-error{{static property 'immutableNonisolated' is not concurrency-safe because non-'Sendable' type 'TestNonsendable' may have shared mutable state}}
  // expected-note@-1 {{isolate 'immutableNonisolated' to a global actor, or conform 'TestNonsendable' to 'Sendable'}}
  // expected-error@-2 {{'nonisolated' can not be applied to variable with non-'Sendable' type 'TestNonsendable'}}
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

func testImportedGlobals() { // expected-note{{add '@MainActor' to make global function 'testImportedGlobals()' part of global actor 'MainActor'}}
  let _ = Globals.integerConstant
  let _ = Globals.integerMutable
  let _ = Globals.nonisolatedUnsafeIntegerConstant
  let _ = Globals.nonisolatedUnsafeIntegerMutable
  let _ = Globals.actorInteger // expected-error{{main actor-isolated static property 'actorInteger' can not be referenced from a non-isolated context}}
}
