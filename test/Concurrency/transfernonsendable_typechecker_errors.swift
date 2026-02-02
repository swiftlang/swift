// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -verify %s -o /dev/null -enable-upcoming-feature GlobalActorIsolatedTypesUsability

// REQUIRES: concurrency
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability

// This test validates that for specific test cases around closures, we properly
// emit errors in the type checker before we run sns. This ensures that we know that
// these cases can't happen when SNS is enabled.

class NonSendableKlass { // expected-note 2{{}}
  var field: NonSendableKlass? = nil
  var boolean: Bool = false

  init() {}
  init(_ x: NonSendableKlass) {
  }

  func asyncCall() async {}
  func asyncCallWithIsolatedParameter(isolation: isolated (any Actor)? = #isolation) async {
  }
}

@MainActor func transferToMain<T>(_ t: T) async {}

var booleanFlag: Bool { false }

class SendableKlass : @unchecked Sendable {}

actor MyActor {
  var klass = NonSendableKlass()
  // expected-note @-1 7{{property declared here}}
  final var finalKlass = NonSendableKlass()

  func useKlass(_ x: NonSendableKlass) {}

  func useSendableFunction(_: @Sendable () -> Void) {}
  func useNonSendableFunction(_: () -> Void) {}
  func doSomething() {}
  @MainActor func useKlassMainActor(_ x: NonSendableKlass) {}
}

struct SingleFieldKlassBox { // expected-complete-note 2{{consider making struct 'SingleFieldKlassBox' conform to the 'Sendable' protocol}}
  var k = NonSendableKlass()
}

struct TwoFieldKlassBox { // expected-note 2{{}}
  var k1 = NonSendableKlass()
  var k2 = NonSendableKlass()
}

class TwoFieldKlassClassBox {
  var k1 = NonSendableKlass()
  var k2 = NonSendableKlass()
  var recursive: TwoFieldKlassClassBox? = nil
}

struct SendableGenericStruct : Sendable {
  var x = SendableKlass()
}

enum MyEnum<T> {
    case none
    indirect case some(NonSendableKlass)
    case more(T)
}

///////////
// Tests //
///////////

extension MyActor {
  func simpleClosureCaptureSelfThroughTupleWithFieldAccess() async {
    // In this case, we erase that we accessed self so we hit a type checker
    // error. We could make this a SNS error, but since in the other cases where
    // we have a nonisolated non-async we are probably going to change it to be
    // async as well making these errors go away.
    let x = (self, 1)
    let closure: () -> () = {
      print(x.0.klass) // expected-error {{actor-isolated property 'klass' can not be referenced from a nonisolated context}}
    }
    await transferToMain(closure)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type '() -> ()' into main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }
}

func testSimpleLetClosureCaptureActorField() async {
  let a = MyActor()
  let closure = { print(a.klass) } // expected-error {{actor-isolated property 'klass' can not be referenced from a nonisolated context}}
  await transferToMain(closure)
  // expected-complete-warning @-1 {{passing argument of non-Sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

func testSimpleLetClosureCaptureActorFieldThroughTuple() async {
  let a = (MyActor(), 0)
  let closure = { print(a.0.klass) } // expected-error {{actor-isolated property 'klass' can not be referenced from a nonisolated context}}
  await transferToMain(closure)
  // expected-complete-warning @-1 {{passing argument of non-Sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

func testSimpleLetClosureCaptureActorFieldThroughOptional() async {
  let a: MyActor? = MyActor()
  let closure = { print(a!.klass) } // expected-error {{actor-isolated property 'klass' can not be referenced from a nonisolated context}}
  await transferToMain(closure)
  // expected-complete-warning @-1 {{passing argument of non-Sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

func testSimpleVarClosureCaptureActorField() async {
  let a = MyActor()
  var closure = {}
  closure = { print(a.klass) } // expected-error {{actor-isolated property 'klass' can not be referenced from a nonisolated context}}
  await transferToMain(closure)
  // expected-complete-warning @-1 {{passing argument of non-Sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

func testSimpleVarClosureCaptureActorFieldThroughTuple() async {
  let a = (MyActor(), 0)
  var closure = {}
  closure = { print(a.0.klass) } // expected-error {{actor-isolated property 'klass' can not be referenced from a nonisolated context}}
  await transferToMain(closure)
  // expected-complete-warning @-1 {{passing argument of non-Sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

func testSimpleVarClosureCaptureActorFieldThroughOptional() async {
  let a: MyActor? = MyActor()
  var closure = {}
  closure = { print(a!.klass) } // expected-error {{actor-isolated property 'klass' can not be referenced from a nonisolated context}}
  await transferToMain(closure)
  // expected-complete-warning @-1 {{passing argument of non-Sendable type '() -> ()' into main actor-isolated context may introduce data races}}
  // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

actor ActorWithSetter {
  var field = NonSendableKlass()
  var twoFieldBox = TwoFieldKlassBox()
  var twoFieldBoxInTuple = (NonSendableKlass(), TwoFieldKlassBox())
  var recursive: ActorWithSetter? = nil
  var classBox = TwoFieldKlassClassBox()

  // This triggers a crash in SILGen with tns enabled.
  func recursive() async {
    let x = NonSendableKlass()
    await self.recursive!.twoFieldBoxInTuple.1.k2 = x
    // expected-warning @-1 {{non-Sendable type '(NonSendableKlass, TwoFieldKlassBox)' of property 'twoFieldBoxInTuple' cannot exit actor-isolated context}}
    // expected-warning @-2 {{non-Sendable type '(NonSendableKlass, TwoFieldKlassBox)' of property 'twoFieldBoxInTuple' cannot exit actor-isolated context}}

    await transferToMain(x) // xpected-tns-warning {{call site passes `self` or a non-Sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  }
}

final actor FinalActorWithSetter {
  var field = NonSendableKlass()
  var twoFieldBox = TwoFieldKlassBox()
  var twoFieldBoxInTuple = (NonSendableKlass(), TwoFieldKlassBox())
  var recursive: ActorWithSetter? = nil
  var classBox = TwoFieldKlassClassBox()

  // This triggers a crash in SILGen with tns enabled.
  func recursive() async {
    let x = NonSendableKlass()
    await self.recursive!.twoFieldBoxInTuple.1.k2 = x
    // expected-warning @-1 {{non-Sendable type '(NonSendableKlass, TwoFieldKlassBox)' of property 'twoFieldBoxInTuple' cannot exit actor-isolated context}}
    // expected-warning @-2 {{non-Sendable type '(NonSendableKlass, TwoFieldKlassBox)' of property 'twoFieldBoxInTuple' cannot exit actor-isolated context}}

    await transferToMain(x) // xpected-tns-warning {{call site passes `self` or a non-Sendable argument of this function to another thread, potentially yielding a race with the caller}}
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  }
}
