// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -swift-version 6 -module-name implicit_nonisolated_things -o %t/implicit_nonisolated_things.swiftmodule %S/Inputs/implicit_nonisolated_things.swift
// RUN: %target-swift-frontend -I %t -swift-version 5 -emit-sil -default-isolation MainActor %s -verify -verify-additional-prefix swift5-
// RUN: %target-swift-frontend -I %t  -swift-version 6 -emit-sil -default-isolation MainActor %s -verify -verify-additional-prefix swift6-

// READ THIS! This test is meant to check the specific isolation when
// `-default-isolation` is set to `MainActor` in combination with validating
// behavior around explicitly non-Sendable types that trigger type checker
// specific errors. Please do not put other types of tests in here.

import implicit_nonisolated_things

// Fake Sendable Data
class SendableData : @unchecked Sendable {}
// expected-swift5-note@-1 {{calls to initializer 'init()' from outside of its actor context are implicitly asynchronous}}

nonisolated func getDataFromSocket() -> SendableData { SendableData() }
// expected-swift5-warning@-1 {{call to main actor-isolated initializer 'init()' in a synchronous nonisolated context}}

class Klass { // expected-swift5-note 3 {{}} expected-swift6-note 3 {{}}
  let s = SendableData()
  // expected-swift5-note@-1 2 {{}}

  init() { s = SendableData() } // expected-swift5-error {{immutable value 'self.s' may only be initialized once}}
  init(_ s: SendableData) {}

  func doSomething() {}
}

@available(*, unavailable)
extension Klass : Sendable {}

struct StructContainingKlass {
  var k = Klass()
}

func unspecifiedAsync<T>(_ t: T) async {}
nonisolated func nonisolatedAsync<T>(_ t: T) async {}
@MainActor func mainActorAsync<T>(_ t: T) async {}

func unspecifiedFunctionTest() async {
  let k = Klass()
  await unspecifiedAsync(k)
  await nonisolatedAsync(k)
  await mainActorAsync(k)
}

func unspecifiedFunctionTest2() async {
  let k = StructContainingKlass()
  await unspecifiedAsync(k)
  await nonisolatedAsync(k)
  await mainActorAsync(k)

  await unspecifiedAsync(k.k)
  await nonisolatedAsync(k.k)
  await mainActorAsync(k.k)
}

nonisolated func nonisolatedFunctionTest() async {
  let k = await StructContainingKlass()
  await unspecifiedAsync(k.k)
  // expected-swift5-warning@-1 {{non-Sendable type 'Klass' of property 'k' cannot exit main actor-isolated context}}
  // expected-swift6-error@-2 {{non-Sendable type 'Klass' of property 'k' cannot exit main actor-isolated context}}
  await nonisolatedAsync(k.k)
  // expected-swift5-warning@-1 {{non-Sendable type 'Klass' of property 'k' cannot exit main actor-isolated context}}
  // expected-swift6-error@-2 {{non-Sendable type 'Klass' of property 'k' cannot exit main actor-isolated context}}
  await mainActorAsync(k.k)
  // expected-swift5-warning@-1 {{non-Sendable type 'Klass' of property 'k' cannot exit main actor-isolated context}}
  // expected-swift6-error@-2 {{non-Sendable type 'Klass' of property 'k' cannot exit main actor-isolated context}}
}

func testTask() async {
  Task {
    let k = Klass(getDataFromSocket())
    k.doSomething()
  }
}

func testTaskDetached() async {
  Task.detached {
    let k = await Klass(getDataFromSocket())
    // Have to pop back onto the main thread to do something.
    await k.doSomething()
  }
}

// @MainActor
extension Int {
  func memberOfInt() { } // expected-note 3{{calls to instance method 'memberOfInt()' from outside of its actor context are implicitly asynchronous}}
}

nonisolated func testMemberOfInt(i: Int) {
  i.memberOfInt() // expected-swift5-warning{{call to main actor-isolated instance method 'memberOfInt()' in a synchronous nonisolated context}}
  // expected-swift6-error@-1{{call to main actor-isolated instance method 'memberOfInt()' in a synchronous nonisolated context}}
}

protocol SendableProto: Sendable { }

struct MyStruct: SendableProto { }

extension MyStruct: CustomStringConvertible {
  var description: String {
    17.memberOfInt() // okay, on main actor
    return "hello"
  }
}

nonisolated struct MyOtherStruct { }

extension MyOtherStruct {
  func f() {
    17.memberOfInt() // okay, on main actor
  }
}

nonisolated
extension MyOtherStruct {
  func g() {
    17.memberOfInt() // expected-swift5-warning{{call to main actor-isolated instance method 'memberOfInt()' in a synchronous nonisolated context}}
  // expected-swift6-error@-1{{call to main actor-isolated instance method 'memberOfInt()' in a synchronous nonisolated context}}
  }
}

nonisolated protocol P {
  func g()
}

struct MyP: P {
  func g() {
    17.memberOfInt() // okay, on main actor
  }
}

// Above tests for imported types
extension ImportedStruct: @retroactive CustomStringConvertible {
  public var description: String {
    17.memberOfInt() // okay, on main actor
    return "hello"
  }
}

extension ImportedOtherStruct {
  func f() {
    17.memberOfInt() // okay, on main actor
  }
}

nonisolated
extension ImportedOtherStruct {
  func g() {
    17.memberOfInt() // expected-swift5-warning{{call to main actor-isolated instance method 'memberOfInt()' in a synchronous nonisolated context}}
  // expected-swift6-error@-1{{call to main actor-isolated instance method 'memberOfInt()' in a synchronous nonisolated context}}
  }
}

struct MyImportedP: ImportedP {
  func g() {
    17.memberOfInt() // okay, on main actor
  }
}

// https://github.com/swiftlang/swift/issues/82168 -
nonisolated protocol OtherP {
  associatedtype AT
  static var at: AT { get }
}

nonisolated struct KP<R: OtherP, V> {
  init(keyPath: KeyPath<R, V>) {}
}

struct S: OtherP {
  let p: Int
  struct AT {
    let kp = KP(keyPath: \S.p)
  }

  // FIXME: This should not be an error.
  static let at = AT() // expected-swift6-error{{'S.AT' cannot be constructed because it has no accessible initializers}}
}
