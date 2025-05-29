// RUN: %target-swift-frontend -emit-sil -parse-as-library -strict-concurrency=complete -target %target-swift-5.1-abi-triple -verify %s -o /dev/null -enable-upcoming-feature GlobalActorIsolatedTypesUsability

// REQUIRES: concurrency
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {} // expected-note 2{{}}

struct NonSendableStruct {
  var first = NonSendableKlass()
  var second = NonSendableKlass()
}

func getValue<T>() -> T { fatalError() }
func getValueAsync<T>() async -> T { fatalError() }
func getValueAsyncTransferring<T>() async -> sending T { fatalError() }
@MainActor func getMainActorValueAsync<T>() async -> T { fatalError() }
@MainActor func getMainActorValueAsyncTransferring<T>() async -> sending T { fatalError() }

func useValue<T>(_ t: T) {}
func getAny() -> Any { fatalError() }

actor Custom {
}

@globalActor
struct CustomActor {
    static var shared: Custom {
        return Custom()
    }
}

@MainActor func transferToMainIndirect<T>(_ t: T) async {}
@CustomActor func transferToCustomIndirect<T>(_ t: T) async {}
@MainActor func transferToMainDirect(_ t: NonSendableKlass) async {}
@CustomActor func transferToCustomDirect(_ t: NonSendableKlass) async {}
func useValueIndirect<T>(_ t: T) {}
func useValueDirect(_ t: NonSendableKlass) {}

func transferValueDirect(_ x: sending NonSendableKlass) {}
func transferValueIndirect<T>(_ x: sending T) {}

func transferResult() -> sending NonSendableKlass { NonSendableKlass() }
func transferResultWithArg(_ x: NonSendableKlass) -> sending NonSendableKlass { NonSendableKlass() }
func transferResultWithTransferringArg(_ x: sending NonSendableKlass) -> sending NonSendableKlass { NonSendableKlass() }
func transferResultWithTransferringArg2(_ x: sending NonSendableKlass, _ y: NonSendableKlass) -> sending NonSendableKlass { NonSendableKlass() }
func transferResultWithTransferringArg2Throwing(_ x: sending NonSendableKlass, _ y: NonSendableKlass) throws -> sending NonSendableKlass { NonSendableKlass() }

func transferAsyncResult() async -> sending NonSendableKlass { NonSendableKlass() }
func transferAsyncResultWithArg(_ x: NonSendableKlass) async -> sending NonSendableKlass { NonSendableKlass() }
func transferAsyncResultWithTransferringArg(_ x: sending NonSendableKlass) async -> sending NonSendableKlass { NonSendableKlass() }
func transferAsyncResultWithTransferringArg2(_ x: sending NonSendableKlass, _ y: NonSendableKlass) async -> sending NonSendableKlass { NonSendableKlass() }
func transferAsyncResultWithTransferringArg2Throwing(_ x: sending NonSendableKlass, _ y: NonSendableKlass) async throws -> sending NonSendableKlass { NonSendableKlass() }

@MainActor func transferAsyncResultMainActor() async -> sending NonSendableKlass { NonSendableKlass() }

@MainActor var globalNonSendableKlass = NonSendableKlass()

@MainActor
struct MainActorIsolatedStruct {
  let ns = NonSendableKlass()
}

@MainActor
enum MainActorIsolatedEnum {
  case first
  case second(NonSendableKlass)
}

struct GenericNonSendableStruct<T> {
  var t: T
  var t2: T?
  var x: NonSendableKlass
}

class GenericNonSendableKlass<T> {
  var t: T
  var t2: T?
  var x: NonSendableKlass?

  init(_ inputT: T) {
    t = inputT
    t2 = nil
    x = NonSendableKlass()
  }
}

func sendParameter<T>(_ t: sending T) {}

actor MyActor {
  private var ns = NonSendableKlass()
}

/////////////////
// MARK: Tests //
/////////////////

func simpleTest() async {
  let x = NonSendableKlass()
  let y = transferResultWithArg(x)
  await transferToMainDirect(x)
  useValue(y)
}

// Since y is transferred, we should emit the error on useValue(x). We generally
// emit the first seen error on a path, so if we were to emit an error on
// useValue(y), we would have emitted that error.
func simpleTest2() async {
  let x = NonSendableKlass()
  let y = transferResultWithArg(x)
  await transferToMainDirect(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(y)
  useValue(x) // expected-note {{access can happen concurrently}}
}

// Make sure that later errors with y can happen.
func simpleTest3() async {
  let x = NonSendableKlass()
  let y = transferResultWithArg(x)
  await transferToMainDirect(x)
  await transferToMainDirect(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(y) // expected-note {{access can happen concurrently}}
}

func transferResult() async -> sending NonSendableKlass {
  let x = NonSendableKlass()
  await transferToMainDirect(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and local nonisolated uses}}
  return x // expected-note {{access can happen concurrently}}
}

func transferInAndOut(_ x: sending NonSendableKlass) -> sending NonSendableKlass {
  x
}


func transferReturnArg(_ x: NonSendableKlass) -> sending NonSendableKlass {
  return x // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{task-isolated 'x' cannot be a 'sending' result. task-isolated uses may race with caller uses}}
}

// TODO: This will be fixed once I represent @MainActor on func types.
@MainActor func transferReturnArgMainActor(_ x: NonSendableKlass) -> sending NonSendableKlass {
  return x // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{main actor-isolated 'x' cannot be a 'sending' result. main actor-isolated uses may race with caller uses}}
}

// This is safe since we are returning the whole tuple fresh. In contrast,
// (transferring NonSendableKlass, sending NonSendableKlass) would not be
// safe if we ever support that.
func transferReturnArgTuple(_ x: sending NonSendableKlass) -> sending (NonSendableKlass, NonSendableKlass) {
  return (x, x)
}

func useTransferredResultMainActor() async {
  let _ = await transferAsyncResultMainActor()
}

func useTransferredResult() async {
  let _ = await transferAsyncResult()
}

extension MainActorIsolatedStruct {
  func testNonSendableErrorReturnWithTransfer() -> sending NonSendableKlass {
    return ns // expected-warning {{sending 'self.ns' risks causing data races}}
    // expected-note @-1 {{main actor-isolated 'self.ns' cannot be a 'sending' result. main actor-isolated uses may race with caller uses}}
  }
  func testNonSendableErrorReturnNoTransfer() -> NonSendableKlass {
    return ns
  }
}

extension MainActorIsolatedEnum {
  func testSwitchReturn() -> sending NonSendableKlass? {
    switch self {
    case .first:
      return nil
    case .second(let ns):
      return ns
    }
  } // expected-warning {{sending 'ns.some' risks causing data races}}
  // expected-note @-1 {{main actor-isolated 'ns.some' cannot be a 'sending' result. main actor-isolated uses may race with caller uses}}

  func testSwitchReturnNoTransfer() -> NonSendableKlass? {
    switch self {
    case .first:
      return nil
    case .second(let ns):
      return ns
    }
  }

  func testIfLetReturn() -> sending NonSendableKlass? {
    if case .second(let ns) = self {
      return ns // TODO: The error below should be here.
    }
    return nil
  } // expected-warning {{sending 'ns.some' risks causing data races}}
  // expected-note @-1 {{main actor-isolated 'ns.some' cannot be a 'sending' result. main actor-isolated uses may race with caller uses}}

  func testIfLetReturnNoTransfer() -> NonSendableKlass? {
    if case .second(let ns) = self {
      return ns
    }
    return nil
  }

}

///////////////////////////
// MARK: Async Let Tests //
///////////////////////////
//
// Move these tests to async let once strict-concurrency=complete requires
// transfer non sendable.

// Make sure that we can properly construct a reabstraction thunk since
// constructNonSendableKlassAsync doesn't return the value sending but
// async let wants it to be transferring.
//
// Importantly, we should only emit the sema error here saying that one cannot
// return a non-Sendable value here.
func asyncLetReabstractionThunkTest() async {
  // With thunk.
  async let newValue: NonSendableKlass = await getValueAsync()
  let _ = await newValue

  // Without thunk.
  async let newValue2: NonSendableKlass = await getValueAsyncTransferring()
  let _ = await newValue2
}

func asyncLetReabstractionThunkTest2() async {
  // We emit the error here since we are returning a MainActor-isolated value.
  async let newValue: NonSendableKlass = await getMainActorValueAsync()
  // expected-warning @-1 {{non-Sendable 'NonSendableKlass'-typed result can not be returned from main actor-isolated global function 'getMainActorValueAsync()' to nonisolated context}}

  let _ = await newValue

  // Without thunk.
  async let newValue2: NonSendableKlass = await getMainActorValueAsyncTransferring()
  let _ = await newValue2
}

@MainActor func asyncLetReabstractionThunkTestGlobalActor() async {
  // With thunk we do not emit an error since our async let is not main actor
  // isolated despite being in an @MainActor function.
  async let newValue: NonSendableKlass = await getValueAsync()
  let _ = await newValue

  // Without thunk.
  async let newValue2: NonSendableKlass = await getValueAsyncTransferring()
  let _ = await newValue2
}

@MainActor func asyncLetReabstractionThunkTestGlobalActor2() async {
  // We emit the error here since we are returning a MainActor-isolated value.
  async let newValue: NonSendableKlass = await getMainActorValueAsync()
  // expected-warning @-1 {{non-Sendable 'NonSendableKlass'-typed result can not be returned from main actor-isolated global function 'getMainActorValueAsync()' to nonisolated context}}

  let _ = await newValue

  // Without thunk.
  async let newValue2: NonSendableKlass = await getMainActorValueAsyncTransferring()
  let _ = await newValue2
}

///////////////////////////////////
// MARK: Indirect Sending Result //
///////////////////////////////////

func indirectSending<T>(_ t: T) -> sending T {
  return t // expected-warning {{returning task-isolated 't' as a 'sending' result risks causing data races}}
  // expected-note @-1 {{returning task-isolated 't' risks causing data races since the caller assumes that 't' can be safely sent to other isolation domains}}
}

func indirectSendingStructField<T>(_ t: GenericNonSendableStruct<T>) -> sending T {
  return t.t // expected-warning {{returning task-isolated 't.t' as a 'sending' result risks causing data races}}
  // expected-note @-1 {{returning task-isolated 't.t' risks causing data races since the caller assumes that 't.t' can be safely sent to other isolation domains}}
}

func indirectSendingStructOptionalField<T>(_ t: GenericNonSendableStruct<T>) -> sending T {
  return t.t2! // expected-warning {{returning task-isolated 't.t2.some' as a 'sending' result risks causing data races}}
  // expected-note @-1 {{returning task-isolated 't.t2.some' risks causing data races since the caller assumes that 't.t2.some' can be safely sent to other isolation domains}}
}

func indirectSendingClassField<T>(_ t: GenericNonSendableKlass<T>) -> sending T {
  return t.t // expected-warning {{returning task-isolated 't.t' as a 'sending' result risks causing data races}}
  // expected-note @-1 {{returning task-isolated 't.t' risks causing data races since the caller assumes that 't.t' can be safely sent to other isolation domains}}
}

func indirectSendingOptionalClassField<T>(_ t: GenericNonSendableKlass<T>) -> sending T {
  return t.t2! // expected-warning {{returning a task-isolated 'Optional<T>' value as a 'sending' result risks causing data races}}
  // expected-note @-1 {{returning a task-isolated 'Optional<T>' value risks causing races since the caller assumes the value can be safely sent to other isolation domains}}
  // expected-note @-2 {{'Optional<T>' is a non-Sendable type}}
}

func useBlock<T>(block: () throws -> T) throws -> sending T {
  fatalError()
}

extension MyActor {
  // This shouldn't emit any errors. We used to error on returning result.
  public func withContext<T>(_ block: sending () throws -> T) async throws -> sending T {
    let value: T = try useBlock {
      _ = ns
      return try block()
    }

    return value
  }
}
