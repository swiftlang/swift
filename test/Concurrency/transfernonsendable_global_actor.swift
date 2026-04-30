// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -target %target-swift-5.1-abi-triple -verify -verify-additional-prefix ni- %s -o /dev/null -parse-as-library -enable-upcoming-feature GlobalActorIsolatedTypesUsability
// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -target %target-swift-5.1-abi-triple -verify -verify-additional-prefix ni-ns- %s -o /dev/null -parse-as-library -enable-upcoming-feature GlobalActorIsolatedTypesUsability -enable-upcoming-feature NonisolatedNonsendingByDefault

// REQUIRES: concurrency
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {}
final class SendableKlass : Sendable {}

actor CustomActorInstance {
  func acceptValue(_ x: NonSendableKlass) {}
}

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

func mergeValues(_ x: NonSendableKlass, _ y: NonSendableKlass) {}

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

  await transferToMainActor(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending global actor 'CustomActor'-isolated 'x' to main actor-isolated global function 'transferToMainActor' risks causing data races between main actor-isolated and global actor 'CustomActor'-isolated uses}}
 

  let y = secondList.listHead!.next!

  await transferToMainActor(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending global actor 'CustomActor'-isolated 'y' to main actor-isolated global function 'transferToMainActor' risks causing data races between main actor-isolated and global actor 'CustomActor'-isolated uses}}
 
}

@CustomActor func useCustomActor2() async {
  var x = NonSendableLinkedListNode<Int>()

  if booleanFlag {
    x = secondList.listHead!.next!
  }

  await transferToMainActor(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending global actor 'CustomActor'-isolated 'x' to main actor-isolated global function 'transferToMainActor' risks causing data races between main actor-isolated and global actor 'CustomActor'-isolated uses}}
 
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

  await transferToNonIsolated(x) // expected-ni-warning {{sending 'x' risks causing data races}}
  // expected-ni-note @-1 {{sending global actor 'CustomActor'-isolated 'x' to @concurrent global function 'transferToNonIsolated' risks causing data races between @concurrent and global actor 'CustomActor'-isolated uses}}
 

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

  await transferToNonIsolated(x) // expected-ni-warning {{sending 'x' risks causing data races}}
  // expected-ni-note @-1 {{sending global actor 'CustomActor'-isolated 'x' to @concurrent global function 'transferToNonIsolated' risks causing data races between @concurrent and global actor 'CustomActor'-isolated uses}}
 
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
  let _: Int = await c.measure { // expected-ni-warning {{sending value of non-Sendable type '() async -> Int' risks causing data races}}
    // expected-ni-note @-1 {{sending main actor-isolated value of non-Sendable type '() async -> Int' to @concurrent instance method 'measure' risks causing races in between main actor-isolated and @concurrent uses}}
    try! await c.sleep()
  }
}

@CustomActor func testGlobalAndGlobalIsolatedPartialApplyMismatch() {
  let ns = customActorIsolatedGlobal

  let _ = { @MainActor in
    print(ns) // expected-warning {{sending 'ns' risks causing data races}}
    // expected-note @-1 {{global actor 'CustomActor'-isolated 'ns' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later global actor 'CustomActor'-isolated uses}}
   
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

  await useValueAsync(erased) // expected-ni-warning {{sending 'erased' risks causing data races}}
  // expected-ni-note @-1 {{sending main actor-isolated 'erased' to @concurrent global function 'useValueAsync' risks causing data races between @concurrent and main actor-isolated uses}}
 
 
}

@MainActor func synchronousActorIsolatedFunctionError() async {
  let erased: () -> Void = mainActorFunction

  await useValueAsync(erased) // expected-ni-warning {{sending 'erased' risks causing data races}}
  // expected-ni-note @-1 {{sending main actor-isolated 'erased' to @concurrent global function 'useValueAsync' risks causing data races between @concurrent and main actor-isolated uses}}
 
 
}

@MainActor func synchronousActorIsolatedGenericFunctionError<T>(_ t: T) async {
  let erased: (T) -> Void = useValueMainActor

  await useValueAsync(erased) // expected-ni-warning {{sending 'erased' risks causing data races}}
  // expected-ni-note @-1 {{sending main actor-isolated 'erased' to @concurrent global function 'useValueAsync' risks causing data races between @concurrent and main actor-isolated uses}}
}

@MainActor func synchronousActorIsolatedClassMethodError() async {
  @MainActor class Test {
    func foo() {}
  }

  let t = Test()
  let erased: () -> Void = t.foo

  await useValueAsync(erased) // expected-ni-warning {{sending 'erased' risks causing data races}}
  // expected-ni-note @-1 {{sending main actor-isolated 'erased' to @concurrent global function 'useValueAsync' risks causing data races between @concurrent and main actor-isolated uses}} 
}

@MainActor func synchronousActorIsolatedFinalClassMethodError() async {
  @MainActor final class Test {
    func foo() {}
  }

  let t = Test()
  let erased: () -> Void = t.foo

  await useValueAsync(erased) // expected-ni-warning {{sending 'erased' risks causing data races}}
  // expected-ni-note @-1 {{sending main actor-isolated 'erased' to @concurrent global function 'useValueAsync' risks causing data races between @concurrent and main actor-isolated uses}}
}

@MainActor func synchronousClosureCapturingGlobalActorIsolatedGlobal() async {
  let closure = {
    print(mainActorIsolatedGlobal)
  }
  // Regions: [{(closure), @MainActor}]
  await transferToCustomActor(closure) // expected-warning {{sending 'closure' risks causing data races}}
  // expected-note @-1 {{sending main actor-isolated 'closure' to global actor 'CustomActor'-isolated global function 'transferToCustomActor' risks causing data races between global actor 'CustomActor'-isolated and main actor-isolated uses}}
 
 
}

@MainActor func synchronousClosureCapturingGlobalActorIsolatedFunction() async {
  let closure = {
    mainActorFunction()
  }
  await transferToCustomActor(closure) // expected-warning {{sending 'closure' risks causing data races}}
  // expected-note @-1 {{sending main actor-isolated 'closure' to global actor 'CustomActor'-isolated global function 'transferToCustomActor' risks causing data races between global actor 'CustomActor'-isolated and main actor-isolated uses}}
 
 
}

@MainActor
func localCaptureDataRace5() {
  var x = 0
  _ = x

  Task.detached { @CustomActor in x = 1 } // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{'x' is captured by a global actor 'CustomActor'-isolated closure. global actor 'CustomActor'-isolated uses in closure may race against later main actor-isolated uses}}

  x = 2 // expected-note {{access can happen concurrently}}
}

func inferLocationOfCapturedActorIsolatedSelfCorrectly() {
  class A {
    var block:  @MainActor () -> Void = {}
  }
  @CustomActor
  class B {
    let a = A()

    func d() {
      a.block = c // Ok
    }

    @MainActor
    func c() {}
  }
}

// We shouldn't emit any diagnostic here and shouldn't emit a compiler doesn't
// know how to process error.
actor PreferIsolationOfFieldToIsolationOfActor {
  final class C {
    func foo(_ block: () -> Void) {}
  }

  @MainActor let c: C = C()
  private var data: UInt8 = 0

  @MainActor
  func bar() async {
    let data = await self.data
    c.foo {
      let data = data
      _ = data
    }
  }
}

// We need to error on this below since ns becomes main actor isolated and then
// we send it into a different actor.
@MainActor
class SetterAssignmentMustInferGlobalIsolationTest {
  var nsField = NonSendableKlass()

  func send() async {
    let ns = NonSendableKlass()
    nsField = ns
    await CustomActor.shared.acceptValue(ns) // expected-warning {{sending 'ns' risks causing data races}}
    // expected-note @-1 {{sending main actor-isolated 'ns' to actor-isolated instance method 'acceptValue' risks causing data races between actor-isolated and main actor-isolated uses}}
  }
}

//////////////////////////////////////////////
// MARK: nonisolated(unsafe) override tests //
//////////////////////////////////////////////

func NonisolatedUnsafeOverrideTests() {

class MyObj {}

  @MainActor
  struct StructWrapper {
    nonisolated(unsafe) var _store: MyObj?
  }

  @MainActor
  struct GenericStructWrapper<T> {
    nonisolated(unsafe) var _store: MyObj?
    var t: T? = nil
  }

  @MainActor
  class ClassWrapper {
    nonisolated(unsafe) var _store: MyObj?
  }

  @MainActor
  final class FinalClassWrapper {
    nonisolated(unsafe) var _store: MyObj?
  }

  struct Box {
    var oldStore: MyObj?

    func structTest(w: StructWrapper) {
      let _ = oldStore === w._store
    }

    func genericStructTest<T>(w: GenericStructWrapper<T>) {
      let _ = oldStore === w._store
    }

    func classTest(w: ClassWrapper) {
      let _ = oldStore === w._store
    }

    func finalClassTest(w: FinalClassWrapper) {
      let _ = oldStore === w._store
    }
  }

  @MainActor
  struct MainActorStruct {
    nonisolated(unsafe) var field: NonSendableKlass? = nil
    var field2: NonSendableKlass? = nil
    @CustomActor var customField: NonSendableKlass? = nil

    init() {
      mergeValues(field!, customField!)
      mergeValues(field2!, customField!) // expected-warning {{passing global actor 'CustomActor'-isolated 'self.customField' and main actor-isolated 'self.field2' as arguments to local function 'mergeValues' risks causing data races}}
      // expected-note @-1 {{'self.field2' could begin referencing 'self.customField' allowing concurrent access to 'self.customField' by main actor-isolated code and global actor 'CustomActor'-isolated code}}
    }

    func testMultipleAccessesAreIndependent() async {
      let x = field
      await transferToCustomActor(field)
      useValue(x)
      await transferToCustomActor(x) // expected-warning {{sending 'x' risks causing data races}}
      // expected-note @-1 {{sending 'x' to global actor 'CustomActor'-isolated global function 'transferToCustomActor' risks causing data races between global actor 'CustomActor'-isolated and local main actor-isolated uses}}
      useValue(x) // expected-note {{access can happen concurrently}}
      useValue(field)
    }
  }

  func mergeValues<T>(_ x: T, _ y: NonSendableKlass) {}

  @MainActor
  struct MainActorIndirectStruct<T> {
    nonisolated(unsafe) var field: T? = nil
    var field2: T? = nil
    @CustomActor var customField: NonSendableKlass? = nil
    init() {
      mergeValues(field!, customField!)
      mergeValues(field2!, customField!) // expected-warning {{passing global actor 'CustomActor'-isolated 'self.customField' and main actor-isolated 'self.field2' as arguments to local function 'mergeValues' risks causing data races}}
      // expected-note @-1 {{'self.field2' could begin referencing 'self.customField' allowing concurrent access to 'self.customField' by main actor-isolated code and global actor 'CustomActor'-isolated code}}
    }

    func testMultipleAccessesAreIndependent() async {
      let x = field
      await transferToCustomActor(field)
      useValue(x)
      await transferToCustomActor(x) // expected-warning {{sending 'x' risks causing data races}}
      // expected-note @-1 {{sending 'x' to global actor 'CustomActor'-isolated global function 'transferToCustomActor' risks causing data races between global actor 'CustomActor'-isolated and local main actor-isolated uses}}
      useValue(x) // expected-note {{access can happen concurrently}}
      useValue(field)
    }
  }

  @MainActor
  final class MainActorFinalClass {
    nonisolated(unsafe) var field: NonSendableKlass? = nil
    var field2: NonSendableKlass? = nil
    @CustomActor var customField: NonSendableKlass? = nil

    init() {
      mergeValues(field!, customField!)
      mergeValues(field2!, customField!) // expected-warning {{passing global actor 'CustomActor'-isolated 'self.customField' and main actor-isolated 'self.field2' as arguments to local function 'mergeValues' risks causing data races}}
      // expected-note @-1 {{'self.field2' could begin referencing 'self.customField' allowing concurrent access to 'self.customField' by main actor-isolated code and global actor 'CustomActor'-isolated code}}
    }

    func testMultipleAccessesAreIndependent() async {
      let x = field
      await transferToCustomActor(field)
      useValue(x)
      await transferToCustomActor(x) // expected-warning {{sending 'x' risks causing data races}}
      // expected-note @-1 {{sending 'x' to global actor 'CustomActor'-isolated global function 'transferToCustomActor' risks causing data races between global actor 'CustomActor'-isolated and local main actor-isolated uses}}
      useValue(x) // expected-note {{access can happen concurrently}}
      useValue(field)
    }
  }

  @MainActor
  class MainActorClass {
    nonisolated(unsafe) var field: NonSendableKlass? = nil
    var field2: NonSendableKlass? = nil
    @CustomActor var customField: NonSendableKlass? = nil

    init() {
      mergeValues(field!, customField!)
      mergeValues(field2!, customField!) // expected-warning {{passing global actor 'CustomActor'-isolated 'self.customField' and main actor-isolated 'self.field2' as arguments to local function 'mergeValues' risks causing data races}}
      // expected-note @-1 {{'self.field2' could begin referencing 'self.customField' allowing concurrent access to 'self.customField' by main actor-isolated code and global actor 'CustomActor'-isolated code}}
    }

    func testMultipleAccessesAreIndependent() async {
      let x = field
      await transferToCustomActor(field)
      useValue(x)
      await transferToCustomActor(x) // expected-warning {{sending 'x' risks causing data races}}
      // expected-note @-1 {{sending 'x' to global actor 'CustomActor'-isolated global function 'transferToCustomActor' risks causing data races between global actor 'CustomActor'-isolated and local main actor-isolated uses}}
      useValue(x) // expected-note {{access can happen concurrently}}
      useValue(field)
    }
  }

  // Test that a differently-isolated context can access a nonisolated(unsafe)
  // field on a @MainActor type without producing spurious cross-isolation merge
  // errors.
  @CustomActor
  func crossActorAccessTest(s: MainActorStruct, c: MainActorClass, fc: MainActorFinalClass) async {
    let x = s.field
    useValue(x)

    let y = c.field
    useValue(y)

    let z = fc.field
    useValue(z)
  }

}

///////////////////////////////////////////////////////////
// MARK: Actor init with nonisolated(unsafe) merge tests //
///////////////////////////////////////////////////////////

actor ActorWithNonisolatedUnsafeField {
  nonisolated(unsafe) var field: NonSendableKlass? = nil
  var field2: NonSendableKlass? = nil
  @MainActor var mainField: NonSendableKlass? = nil

  init() {
    mergeValues(field!, mainField!)
    mergeValues(field2!, mainField!) // expected-warning {{passing main actor-isolated 'self.mainField' and 'self'-isolated 'self.field2' as arguments to global function 'mergeValues' risks causing data races}}
    // expected-note @-1 {{'self.field2' could begin referencing 'self.mainField' allowing concurrent access to 'self.mainField' by 'self'-isolated code and main actor-isolated code}}
  }

  func testMultipleAccessesAreIndependent() async {
    let x = field
    await transferToMainActor(field)
    useValue(x)
    useValue(field)
  }
}
