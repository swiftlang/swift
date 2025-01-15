// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/GlobalVariables.swiftmodule -module-name GlobalVariables -parse-as-library -strict-concurrency=minimal -swift-version 5 %S/Inputs/GlobalVariables.swift
// RUN: %target-swift-frontend -disable-availability-checking -parse-as-library -swift-version 6 -I %t -emit-sil -o /dev/null -verify %s

// REQUIRES: concurrency

@preconcurrency import GlobalVariables

@globalActor
actor TestGlobalActor {
  static let shared = TestGlobalActor()
}

@TestGlobalActor
var mutableIsolatedGlobal = 1

var mutableNonisolatedGlobal = 1 // expected-error{{var 'mutableNonisolatedGlobal' is not concurrency-safe because it is nonisolated global shared mutable state}}
// expected-note@-1{{add '@MainActor' to make var 'mutableNonisolatedGlobal' part of global actor 'MainActor'}}{{1-1=@MainActor }}
// expected-note@-2{{disable concurrency-safety checks if accesses are protected by an external synchronization mechanism}}{{1-1=nonisolated(unsafe) }}
// expected-note@-3{{convert 'mutableNonisolatedGlobal' to a 'let' constant to make 'Sendable' shared state immutable}}{{1-4=let}}

let immutableGlobal = 1

final class TestSendable: Sendable {
  init() {}
}

final class TestNonsendable { // expected-note 3{{class 'TestNonsendable' does not conform to the 'Sendable' protocol}}
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

// https://github.com/apple/swift/issues/71546
actor TestActor {
  nonisolated(unsafe) let immutableActorIsolated = TestSendable()
  // expected-warning@-1 {{'nonisolated(unsafe)' is unnecessary for a constant actor-isolated property with 'Sendable' type 'TestSendable', consider removing it}} {{3-23=}}
}

struct TestStatics {
  static let immutableExplicitSendable = TestSendable()
  static let immutableNonsendable = TestNonsendable() // expected-error{{static property 'immutableNonsendable' is not concurrency-safe because non-'Sendable' type 'TestNonsendable' may have shared mutable state}}
  // expected-note@-1 {{add '@MainActor' to make static property 'immutableNonsendable' part of global actor 'MainActor'}}
  // expected-note@-2 {{disable concurrency-safety checks if accesses are protected by an external synchronization mechanism}}
  static nonisolated(unsafe) let immutableNonisolatedUnsafe = TestNonsendable()
  static nonisolated let immutableNonisolated = TestNonsendable() // expected-error{{static property 'immutableNonisolated' is not concurrency-safe because non-'Sendable' type 'TestNonsendable' may have shared mutable state}}
  // expected-note@-1 {{disable concurrency-safety checks if accesses are protected by an external synchronization mechanism}}
  // expected-error@-2 {{'nonisolated' can not be applied to variable with non-'Sendable' type 'TestNonsendable'}}
  // expected-note@-3{{add '@MainActor' to make static property 'immutableNonisolated' part of global actor 'MainActor'}}
  static nonisolated(unsafe) let immutableNonisolatedUnsafeSendable = TestSendable()
  // expected-warning@-1 {{'nonisolated(unsafe)' is unnecessary for a constant with 'Sendable' type 'TestSendable', consider removing it}} {{10-30=}}
  static let immutableInferredSendable = 0
  static var mutable = 0 // expected-error{{static property 'mutable' is not concurrency-safe because it is nonisolated global shared mutable state}}
  // expected-note@-1{{convert 'mutable' to a 'let' constant to make 'Sendable' shared state immutable}}
  // expected-note@-2{{disable concurrency-safety checks if accesses are protected by an external synchronization mechanism}}
  // expected-note@-3{{add '@MainActor' to make static property 'mutable' part of global actor 'MainActor'}}
  static var computedProperty: Int { 0 } // computed property that, though static, has no storage so is not a global
  @TestWrapper static var wrapped: Int // expected-error{{static property 'wrapped' is not concurrency-safe because it is nonisolated global shared mutable state}}
  // expected-note@-1{{convert 'wrapped' to a 'let' constant to make 'Sendable' shared state immutable}}{{23-26=let}}
  // expected-note@-2{{disable concurrency-safety checks if accesses are protected by an external synchronization mechanism}}{{16-16=nonisolated(unsafe) }}
  // expected-note@-3{{add '@MainActor' to make static property 'wrapped' part of global actor 'MainActor'}}{{3-3=@MainActor }}
}

public actor TestPublicActor {
  nonisolated(unsafe) let immutableNonisolatedUnsafeSendable = TestSendable()
  // expected-warning@-1 {{'(unsafe)' is unnecessary for a constant public actor property with 'Sendable' type 'TestSendable', consider removing it}} {{14-22=}}

  // https://github.com/swiftlang/swift/issues/78435
  static var actorStatic = 0
  // expected-error@-1 {{static property 'actorStatic' is not concurrency-safe because it is nonisolated global shared mutable state}}
  // expected-note@-2{{convert 'actorStatic' to a 'let' constant to make 'Sendable' shared state immutable}}{{10-13=let}}
  // expected-note@-3{{disable concurrency-safety checks if accesses are protected by an external synchronization mechanism}}{{3-3=nonisolated(unsafe) }}
  // expected-note@-4{{add '@MainActor' to make static property 'actorStatic' part of global actor 'MainActor'}}{{3-3=@MainActor }}
}

enum EnumSpace {
  static let enumStaticLet = TestSendable()

  static var enumStatic = 0
  // expected-error@-1 {{static property 'enumStatic' is not concurrency-safe because it is nonisolated global shared mutable state}}
  // expected-note@-2{{convert 'enumStatic' to a 'let' constant to make 'Sendable' shared state immutable}}{{10-13=let}}
  // expected-note@-3{{disable concurrency-safety checks if accesses are protected by an external synchronization mechanism}}{{3-3=nonisolated(unsafe) }}
  // expected-note@-4{{add '@MainActor' to make static property 'enumStatic' part of global actor 'MainActor'}}{{3-3=@MainActor }}
}

@TestGlobalActor
enum IsolatedEnumSpace {
  static let inferredGlobalActorStaticLet = TestSendable()

  static var inferredGlobalActorStatic = 0
}

public actor GlobalActorComputed: GlobalActor {
  static let storage = GlobalActorComputed()

  public static var shared: GlobalActorComputed {
    Self.storage
  }
}

@TestGlobalActor
func f() {
  print(TestStatics.immutableExplicitSendable)
  print(TestStatics.immutableInferredSendable)
  print(TestStatics.mutable)
  print(Globals.actorInteger) // expected-error{{main actor-isolated static property 'actorInteger' can not be referenced from global actor 'TestGlobalActor'}}
}

func testLocalNonisolatedUnsafe() async {
  nonisolated(unsafe) let immutable = 1
  // expected-warning@-1{{'nonisolated(unsafe)' is unnecessary for a constant with 'Sendable' type 'Int', consider removing it}} {{3-23=}}
  // expected-warning@-2{{initialization of immutable value 'immutable' was never used; consider replacing with assignment to '_' or removing it}}
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
  let _ = Globals.actorInteger // expected-error{{main actor-isolated static property 'actorInteger' can not be referenced from a nonisolated context}}
}
