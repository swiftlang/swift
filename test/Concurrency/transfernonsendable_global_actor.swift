// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -verify -verify-additional-prefix complete- %s -o /dev/null -parse-as-library -disable-region-based-isolation-with-strict-concurrency
// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -verify -verify-additional-prefix tns-  %s -o /dev/null -parse-as-library

// REQUIRES: concurrency
// REQUIRES: asserts

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {} // expected-complete-note {{}}
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

/////////////////
// MARK: Tests //
/////////////////

private class NonSendableLinkedList<T> { // expected-complete-note 5{{}}
  var listHead: NonSendableLinkedListNode<T>?

  init() { listHead = nil }
}

private class NonSendableLinkedListNode<T> { // expected-complete-note 3{{}}
  var next: NonSendableLinkedListNode?
  var data: T?

  init() { next = nil }
}

@CustomActor private var firstList = NonSendableLinkedList<Int>()
@CustomActor private var secondList = NonSendableLinkedList<Int>()

@CustomActor func useCustomActor1() async {
  let x = firstList

  await transferToMainActor(x) // expected-tns-warning {{transferring 'x' may cause a data race}}
  // expected-tns-note @-1 {{transferring global actor 'CustomActor'-isolated 'x' to main actor-isolated callee could cause races between main actor-isolated and global actor 'CustomActor'-isolated uses}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableLinkedList<Int>' into main actor-isolated context may introduce data races}}

  let y = secondList.listHead!.next!

  await transferToMainActor(y) // expected-tns-warning {{transferring 'y' may cause a data race}}
  // expected-tns-note @-1 {{transferring global actor 'CustomActor'-isolated 'y' to main actor-isolated callee could cause races between main actor-isolated and global actor 'CustomActor'-isolated uses}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableLinkedListNode<Int>' into main actor-isolated context may introduce data races}}
}

@CustomActor func useCustomActor2() async {
  var x = NonSendableLinkedListNode<Int>()

  if booleanFlag {
    x = secondList.listHead!.next!
  }

  await transferToMainActor(x) // expected-tns-warning {{transferring 'x' may cause a data race}}
  // expected-tns-note @-1 {{transferring global actor 'CustomActor'-isolated 'x' to main actor-isolated callee could cause races between main actor-isolated and global actor 'CustomActor'-isolated uses}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableLinkedListNode<Int>' into main actor-isolated context may introduce data races}}
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

  await transferToNonIsolated(x) // expected-tns-warning {{transferring 'x' may cause a data race}}
  // expected-tns-note @-1 {{transferring disconnected 'x' to nonisolated callee could cause races in between callee nonisolated and local global actor 'CustomActor'-isolated uses}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableLinkedListNode<Int>' outside of global actor 'CustomActor'-isolated context may introduce data races}}

  useValue(x) // expected-tns-note {{use here could race}}
}

private struct StructContainingValue { // expected-complete-note 2{{}}
  var x = NonSendableLinkedList<Int>()
  var y = SendableKlass()
}

@CustomActor func useCustomActor6() async {
  var x = StructContainingValue()
  x = StructContainingValue()

  await transferToNonIsolated(x) // expected-tns-warning {{transferring 'x' may cause a data race}}
  // expected-tns-note @-1 {{transferring disconnected 'x' to nonisolated callee could cause races in between callee nonisolated and local global actor 'CustomActor'-isolated uses}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'StructContainingValue' outside of global actor 'CustomActor'-isolated context may introduce data races}}

  useValue(x) // expected-tns-note {{use here could race}}
}

@CustomActor func useCustomActor7() async {
  var x = StructContainingValue()
  x.x = firstList

  await transferToNonIsolated(x) // expected-tns-warning {{transferring 'x' may cause a data race}}
  // expected-tns-note @-1 {{transferring global actor 'CustomActor'-isolated 'x' to nonisolated callee could cause races between nonisolated and global actor 'CustomActor'-isolated uses}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'StructContainingValue' outside of global actor 'CustomActor'-isolated context may introduce data races}}

  useValue(x)
}

@CustomActor func useCustomActor8() async {
  var x = (NonSendableLinkedList<Int>(), NonSendableLinkedList<Int>())
  x = (NonSendableLinkedList<Int>(), NonSendableLinkedList<Int>())

  await transferToNonIsolated(x) // expected-tns-warning {{transferring 'x' may cause a data race}}
  // expected-tns-note @-1 {{transferring disconnected 'x' to nonisolated callee could cause races in between callee nonisolated and local global actor 'CustomActor'-isolated uses}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type '(NonSendableLinkedList<Int>, NonSendableLinkedList<Int>)' outside of global actor 'CustomActor'-isolated context may introduce data races}}
  // expected-complete-warning @-3 {{passing argument of non-sendable type '(NonSendableLinkedList<Int>, NonSendableLinkedList<Int>)' outside of global actor 'CustomActor'-isolated context may introduce data races}}

  useValue(x) // expected-tns-note {{use here could race}}
}

@CustomActor func useCustomActor9() async {
  var x = (NonSendableLinkedList<Int>(), NonSendableLinkedList<Int>())

  x.1 = firstList

  await transferToNonIsolated(x) // expected-tns-warning {{transferring 'x' may cause a data race}}
  // expected-tns-note @-1 {{transferring global actor 'CustomActor'-isolated 'x' to nonisolated callee could cause races between nonisolated and global actor 'CustomActor'-isolated uses}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type '(NonSendableLinkedList<Int>, NonSendableLinkedList<Int>)' outside of global actor 'CustomActor'-isolated context may introduce data races}}
  // expected-complete-warning @-3 {{passing argument of non-sendable type '(NonSendableLinkedList<Int>, NonSendableLinkedList<Int>)' outside of global actor 'CustomActor'-isolated context may introduce data races}}

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
  let _: Int = await c.measure { // expected-tns-warning {{main actor-isolated value of type '() async -> Int' transferred to nonisolated context}}
    // expected-complete-warning @-1 {{passing argument of non-sendable type '() async -> Int' outside of main actor-isolated context may introduce data races}}
    // expected-complete-note @-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
    try! await c.sleep()
  }
}

@CustomActor func testGlobalAndGlobalIsolatedPartialApplyMismatch() {
  let ns = customActorIsolatedGlobal

  let _ = { @MainActor in
    // TODO: The type checker seems to think that the isolation here is
    // nonisolated instead of custom actor isolated.
    print(ns) // expected-tns-warning {{transferring 'ns' may cause a data race}}
    // expected-tns-note @-1 {{global actor 'CustomActor'-isolated 'ns' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    // expected-complete-warning @-2 {{capture of 'ns' with non-sendable type 'NonSendableKlass' in an isolated closure}}
  }

  useValue(ns)
}

@MainActor func testGlobalAndGlobalIsolatedPartialApplyMatch() {
  let ns = mainActorIsolatedGlobal

  // This is not a transfer since ns is already main actor isolated.
  let _ = { @MainActor in
    print(ns)
  }

  useValue(ns)
}

@MainActor func testGlobalAndGlobalIsolatedPartialApplyMatch2() {
  var ns = (NonSendableKlass(), NonSendableKlass())
  ns.0 = mainActorIsolatedGlobal

  // This is not a transfer since ns is already main actor isolated.
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

  // Since useValue is running in an actor isolated context, it is ok to use the
  // transferred value 'ns' here.
  useValue(ns)
}

@MainActor func synchronousActorIsolatedClosureError() async {
  let closure = { @MainActor @Sendable in
    MainActor.assertIsolated()
  }

  let erased: () -> Void = closure

  await useValueAsync(erased) // expected-tns-warning {{transferring 'erased' may cause a data race}}
  // expected-tns-note @-1 {{transferring main actor-isolated 'erased' to nonisolated callee could cause races between nonisolated and main actor-isolated uses}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type '() -> Void' outside of main actor-isolated context may introduce data races}}
  // expected-complete-note @-3 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

@MainActor func synchronousActorIsolatedFunctionError() async {
  let erased: () -> Void = mainActorFunction

  await useValueAsync(erased) // expected-tns-warning {{transferring 'erased' may cause a data race}}
  // expected-tns-note @-1 {{transferring main actor-isolated 'erased' to nonisolated callee could cause races between nonisolated and main actor-isolated uses}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type '() -> Void' outside of main actor-isolated context may introduce data races}}
  // expected-complete-note @-3 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

@MainActor func synchronousActorIsolatedGenericFunctionError<T>(_ t: T) async {
  let erased: (T) -> Void = useValueMainActor

  await useValueAsync(erased) // expected-tns-warning {{transferring 'erased' may cause a data race}}
  // expected-tns-note @-1 {{transferring main actor-isolated 'erased' to nonisolated callee could cause races between nonisolated and main actor-isolated uses}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type '(T) -> Void' outside of main actor-isolated context may introduce data races}}
  // expected-complete-note @-3 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

@MainActor func synchronousActorIsolatedClassMethodError() async {
  @MainActor class Test {
    func foo() {}
  }

  let t = Test()
  let erased: () -> Void = t.foo

  await useValueAsync(erased) // expected-tns-warning {{transferring 'erased' may cause a data race}}
  // expected-tns-note @-1 {{transferring main actor-isolated 'erased' to nonisolated callee could cause races between nonisolated and main actor-isolated uses}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type '() -> Void' outside of main actor-isolated context may introduce data races}}
  // expected-complete-note @-3 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

@MainActor func synchronousActorIsolatedFinalClassMethodError() async {
  @MainActor final class Test {
    func foo() {}
  }

  let t = Test()
  let erased: () -> Void = t.foo

  await useValueAsync(erased) // expected-tns-warning {{transferring 'erased' may cause a data race}}
  // expected-tns-note @-1 {{transferring main actor-isolated 'erased' to nonisolated callee could cause races between nonisolated and main actor-isolated uses}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type '() -> Void' outside of main actor-isolated context may introduce data races}}
  // expected-complete-note @-3 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

@MainActor func synchronousClosureCapturingGlobalActorIsolatedGlobal() async {
  let closure = {
    print(mainActorIsolatedGlobal)
  }
  // Regions: [{(closure), @MainActor}]
  await transferToCustomActor(closure) // expected-tns-warning {{transferring 'closure' may cause a data race}}
  // expected-tns-note @-1 {{transferring main actor-isolated 'closure' to global actor 'CustomActor'-isolated callee could cause races between global actor 'CustomActor'-isolated and main actor-isolated uses}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type '() -> ()' into global actor 'CustomActor'-isolated context may introduce data races}}
  // expected-complete-note @-3 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

@MainActor func synchronousClosureCapturingGlobalActorIsolatedFunction() async {
  let closure = {
    mainActorFunction()
  }
  await transferToCustomActor(closure) // expected-tns-warning {{transferring 'closure' may cause a data race}}
  // expected-tns-note @-1 {{transferring main actor-isolated 'closure' to global actor 'CustomActor'-isolated callee could cause races between global actor 'CustomActor'-isolated and main actor-isolated uses}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type '() -> ()' into global actor 'CustomActor'-isolated context may introduce data races}}
  // expected-complete-note @-3 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}
