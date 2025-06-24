// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -target %target-swift-5.1-abi-triple -verify -verify-additional-prefix tns-  %s -o /dev/null -parse-as-library -enable-upcoming-feature GlobalActorIsolatedTypesUsability

// REQUIRES: concurrency
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {}
final class SendableKlass : Sendable {}

actor CustomActorInstance {}

@globalActor
struct CustomActor {
  static let shared = CustomActorInstance()
}

func transferToNonIsolated<T>(_ t: T) async {}
@MainActor func transferToMainActor<T>(_ t: T) async {}
@CustomActor func transferToCustomActor<T>(_ t: T) async {}
func useValue<T>(_ t: T) {}
func useValueAsync<T>(_ t: T) async {}
@MainActor func useValueMainActor<T>(_ t: T) {}
@MainActor func mainActorFunction() {}

var booleanFlag: Bool { false }
@MainActor var mainActorIsolatedGlobal = NonSendableKlass()
@CustomActor var customActorIsolatedGlobal = NonSendableKlass()

@MainActor
class NonSendableGlobalActorIsolatedKlass {}

@available(*, unavailable)
extension NonSendableGlobalActorIsolatedKlass: Sendable {}

@MainActor
struct NonSendableGlobalActorIsolatedStruct {
  var k = NonSendableKlass()
}

@available(*, unavailable)
extension NonSendableGlobalActorIsolatedStruct: Sendable {}

@MainActor
enum NonSendableGlobalActorIsolatedEnum {
  case first
  case second(NonSendableKlass)
  case third(SendableKlass)
}

@available(*, unavailable)
extension NonSendableGlobalActorIsolatedEnum: Sendable {}


/////////////////
// MARK: Tests //
/////////////////

private class NonSendableLinkedList<T> {
  var listHead: NonSendableLinkedListNode<T>?

  init() { listHead = nil }
}

private class NonSendableLinkedListNode<T> {
  var next: NonSendableLinkedListNode?
  var data: T?

  init() { next = nil }
}

@CustomActor private var firstList = NonSendableLinkedList<Int>()
@CustomActor private var secondList = NonSendableLinkedList<Int>()

@CustomActor func useCustomActor1() async {
  let x = firstList

  await transferToMainActor(x) // expected-tns-warning {{sending 'x' risks causing data races}}
  // expected-tns-note @-1 {{sending global actor 'CustomActor'-isolated 'x' to main actor-isolated global function 'transferToMainActor' risks causing data races between main actor-isolated and global actor 'CustomActor'-isolated uses}}


  let y = secondList.listHead!.next!

  await transferToMainActor(y) // expected-tns-warning {{sending 'y' risks causing data races}}
  // expected-tns-note @-1 {{sending global actor 'CustomActor'-isolated 'y' to main actor-isolated global function 'transferToMainActor' risks causing data races between main actor-isolated and global actor 'CustomActor'-isolated uses}}

}

@CustomActor func useCustomActor2() async {
  var x = NonSendableLinkedListNode<Int>()

  if booleanFlag {
    x = secondList.listHead!.next!
  }

  await transferToMainActor(x) // expected-tns-warning {{sending 'x' risks causing data races}}
  // expected-tns-note @-1 {{sending global actor 'CustomActor'-isolated 'x' to main actor-isolated global function 'transferToMainActor' risks causing data races between main actor-isolated and global actor 'CustomActor'-isolated uses}}

}

@CustomActor func useCustomActor3() async {
  var x = NonSendableLinkedListNode<Int>()

  if booleanFlag {
    x = secondList.listHead!.next!
  }

  await transferToCustomActor(x)
}

@CustomActor func useCustomActor4() async {
  let x = NonSendableLinkedListNode<Int>()

  await transferToCustomActor(x)

  useValue(x)
}

@CustomActor func useCustomActor5() async {
  let x = NonSendableLinkedListNode<Int>()

  // This is ok since the nonisolated function cannot transfer x, so once we
  // return x will be isolated again.
  await transferToNonIsolated(x)


  useValue(x)
}

private struct StructContainingValue {
  var x = NonSendableLinkedList<Int>()
  var y = SendableKlass()
}

@CustomActor func useCustomActor6() async {
  var x = StructContainingValue()
  x = StructContainingValue()

  // This is ok since the nonisolated function cannot transfer x meaning after
  // we return we know that x will be disconnected upon return as well.
  await transferToNonIsolated(x)


  useValue(x)
}

@CustomActor func useCustomActor7() async {
  var x = StructContainingValue()
  x.x = firstList

  await transferToNonIsolated(x) // expected-tns-warning {{sending 'x' risks causing data races}}
  // expected-tns-note @-1 {{sending global actor 'CustomActor'-isolated 'x' to nonisolated global function 'transferToNonIsolated' risks causing data races between nonisolated and global actor 'CustomActor'-isolated uses}}


  useValue(x)
}

@CustomActor func useCustomActor8() async {
  var x = (NonSendableLinkedList<Int>(), NonSendableLinkedList<Int>())
  x = (NonSendableLinkedList<Int>(), NonSendableLinkedList<Int>())

  // This is safe since the nonisolated function cannot transfer x further.
  await transferToNonIsolated(x)



  useValue(x)
}

@CustomActor func useCustomActor9() async {
  var x = (NonSendableLinkedList<Int>(), NonSendableLinkedList<Int>())

  x.1 = firstList

  await transferToNonIsolated(x) // expected-tns-warning {{sending 'x' risks causing data races}}
  // expected-tns-note @-1 {{sending global actor 'CustomActor'-isolated 'x' to nonisolated global function 'transferToNonIsolated' risks causing data races between nonisolated and global actor 'CustomActor'-isolated uses}}



  useValue(x)
}

struct Clock {
  public func measure<T>(
    _ work: () async throws -> T
  ) async rethrows -> T {
    try await work()
  }

  public func sleep<T>() async throws -> T { fatalError() }
}

// We used to crash when inferring the type for the diagnostic below.
@MainActor func testIndirectParametersHandledCorrectly() async {
  let c = Clock()
  let _: Int = await c.measure { // expected-tns-warning {{sending value of non-Sendable type '() async -> Int' risks causing data races}}
    // expected-tns-note @-1 {{sending main actor-isolated value of non-Sendable type '() async -> Int' to nonisolated instance method 'measure' risks causing races in between main actor-isolated and nonisolated uses}}
    try! await c.sleep()
  }
}

@CustomActor func testGlobalAndGlobalIsolatedPartialApplyMismatch() {
  let ns = customActorIsolatedGlobal

  let _ = { @MainActor in
    print(ns) // expected-tns-warning {{sending 'ns' risks causing data races}}
    // expected-tns-note @-1 {{global actor 'CustomActor'-isolated 'ns' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later global actor 'CustomActor'-isolated uses}}

  }

  useValue(ns)
}

@MainActor func testGlobalAndGlobalIsolatedPartialApplyMatch() {
  let ns = mainActorIsolatedGlobal

  // This is not a transfer since ns is already MainActor isolated.
  let _ = { @MainActor in
    print(ns)
  }

  useValue(ns)
}

@MainActor func testGlobalAndGlobalIsolatedPartialApplyMatch2() {
  var ns = (NonSendableKlass(), NonSendableKlass())
  ns.0 = mainActorIsolatedGlobal

  // This is not a transfer since ns is already MainActor isolated.
  let _ = { @MainActor in
    print(ns)
  }

  useValue(ns)
}

@MainActor func testGlobalAndDisconnected() {
  let ns = NonSendableKlass()

  let _ = { @MainActor in
    print(ns)
  }

  // Since useValue is running in an actor-isolated context, it is ok to use the
  // transferred value 'ns' here.
  useValue(ns)
}

@MainActor func synchronousActorIsolatedClosureError() async {
  let closure = { @MainActor @Sendable in
    MainActor.assertIsolated()
  }

  let erased: () -> Void = closure

  await useValueAsync(erased) // expected-tns-warning {{sending 'erased' risks causing data races}}
  // expected-tns-note @-1 {{sending main actor-isolated 'erased' to nonisolated global function 'useValueAsync' risks causing data races between nonisolated and main actor-isolated uses}}


}

@MainActor func synchronousActorIsolatedFunctionError() async {
  let erased: () -> Void = mainActorFunction

  await useValueAsync(erased) // expected-tns-warning {{sending 'erased' risks causing data races}}
  // expected-tns-note @-1 {{sending main actor-isolated 'erased' to nonisolated global function 'useValueAsync' risks causing data races between nonisolated and main actor-isolated uses}}


}

@MainActor func synchronousActorIsolatedGenericFunctionError<T>(_ t: T) async {
  let erased: (T) -> Void = useValueMainActor

  await useValueAsync(erased) // expected-tns-warning {{sending 'erased' risks causing data races}}
  // expected-tns-note @-1 {{sending main actor-isolated 'erased' to nonisolated global function 'useValueAsync' risks causing data races between nonisolated and main actor-isolated uses}}


}

@MainActor func synchronousActorIsolatedClassMethodError() async {
  @MainActor class Test {
    func foo() {}
  }

  let t = Test()
  let erased: () -> Void = t.foo

  await useValueAsync(erased) // expected-tns-warning {{sending 'erased' risks causing data races}}
  // expected-tns-note @-1 {{sending main actor-isolated 'erased' to nonisolated global function 'useValueAsync' risks causing data races between nonisolated and main actor-isolated uses}}


}

@MainActor func synchronousActorIsolatedFinalClassMethodError() async {
  @MainActor final class Test {
    func foo() {}
  }

  let t = Test()
  let erased: () -> Void = t.foo

  await useValueAsync(erased) // expected-tns-warning {{sending 'erased' risks causing data races}}
  // expected-tns-note @-1 {{sending main actor-isolated 'erased' to nonisolated global function 'useValueAsync' risks causing data races between nonisolated and main actor-isolated uses}}


}

@MainActor func synchronousClosureCapturingGlobalActorIsolatedGlobal() async {
  let closure = {
    print(mainActorIsolatedGlobal)
  }
  // Regions: [{(closure), @MainActor}]
  await transferToCustomActor(closure) // expected-tns-warning {{sending 'closure' risks causing data races}}
  // expected-tns-note @-1 {{sending main actor-isolated 'closure' to global actor 'CustomActor'-isolated global function 'transferToCustomActor' risks causing data races between global actor 'CustomActor'-isolated and main actor-isolated uses}}


}

@MainActor func synchronousClosureCapturingGlobalActorIsolatedFunction() async {
  let closure = {
    mainActorFunction()
  }
  await transferToCustomActor(closure) // expected-tns-warning {{sending 'closure' risks causing data races}}
  // expected-tns-note @-1 {{sending main actor-isolated 'closure' to global actor 'CustomActor'-isolated global function 'transferToCustomActor' risks causing data races between global actor 'CustomActor'-isolated and main actor-isolated uses}}


}

@MainActor
func localCaptureDataRace5() {
  var x = 0
  _ = x

  Task.detached { @CustomActor in x = 1 } // expected-tns-warning {{sending 'x' risks causing data races}}
  // expected-tns-note @-1 {{'x' is captured by a global actor 'CustomActor'-isolated closure. global actor 'CustomActor'-isolated uses in closure may race against later main actor-isolated uses}}

  x = 2 // expected-tns-note {{access can happen concurrently}}
}

func inferLocationOfCapturedActorIsolatedSelfCorrectly() {
  class A {
    var block:  @MainActor () -> Void = {}
  }
  @CustomActor
  class B {
    let a = A()

    func d() {
      a.block = c // expected-warning {{converting non-Sendable function value to '@MainActor @Sendable () -> Void' may introduce data races}}
      // expected-warning @-1 {{non-Sendable '@MainActor () -> ()'-typed result can not be returned from main actor-isolated function to global actor 'CustomActor'-isolated context}}
      // expected-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
    }

    @MainActor
    func c() {}
  }
}
