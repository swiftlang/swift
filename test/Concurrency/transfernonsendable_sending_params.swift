// RUN: %target-swift-frontend -emit-sil -parse-as-library -target %target-swift-5.1-abi-triple -strict-concurrency=complete -verify -verify-additional-prefix ni- %s -o /dev/null -enable-upcoming-feature GlobalActorIsolatedTypesUsability
// RUN: %target-swift-frontend -emit-sil -parse-as-library -target %target-swift-5.1-abi-triple -strict-concurrency=complete -verify -verify-additional-prefix ni-ns- %s -o /dev/null -enable-upcoming-feature GlobalActorIsolatedTypesUsability -enable-upcoming-feature NonisolatedNonsendingByDefault

// REQUIRES: concurrency
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {
  func use() {}
}

struct NonSendableStruct {
  var first = NonSendableKlass()
  var second = NonSendableKlass()
}

class KlassWithNonSendableStructPair {
  var ns1: NonSendableStruct
  var ns2: (NonSendableStruct, NonSendableStruct)

  init() {
    ns1 = NonSendableStruct()
    ns2 = (ns1, ns1)
  }
}

final class FinalKlassWithNonSendableStructPair {
  var ns1: NonSendableStruct
  var ns2: (NonSendableStruct, NonSendableStruct)

  init() {
    ns1 = NonSendableStruct()
    ns2 = (ns1, ns1)
  }
}

func useValue<T>(_ t: T) {}
func useValueAndReturnGeneric<T>(_ t: T) -> T { t }
func useNonSendableKlassAndReturn(_ t: NonSendableKlass) -> NonSendableKlass { t }
func getAny() -> Any { fatalError() }

actor Custom {
}

@globalActor
struct CustomActor {
  static var shared: Custom {
    return Custom()
  }
}

@MainActor func transferToMain<T>(_ t: T) {}
@CustomActor func transferToCustom<T>(_ t: T) {}

func throwingFunction() throws { fatalError() }

func getBool() -> Bool { false }

func transferArg(_ x: sending NonSendableKlass) {
}

func transferArgAsync(_ x: sending NonSendableKlass) async {
}

func transferArgWithOtherParam(_ x: sending NonSendableKlass, _ y: NonSendableKlass) {
}

func transferArgWithOtherParam2(_ x: NonSendableKlass, _ y: sending NonSendableKlass) {
}

func twoTransferArg(_ x: sending NonSendableKlass, _ y: sending NonSendableKlass) {}

@MainActor var globalKlass = NonSendableKlass()

struct MyError : Error {}

func takeClosure(_ x: sending () -> ()) {}
func takeClosureAndParam(_ x: NonSendableKlass, _ y: sending () -> ()) {}

/////////////////
// MARK: Tests //
/////////////////

func testSimpleTransferLet() {
  let k = NonSendableKlass()
  transferArg(k) // expected-warning {{sending 'k' risks causing data races}}
  // expected-note @-1 {{'k' used after being passed as a 'sending' parameter}}
  useValue(k) // expected-note {{access can happen concurrently}}
}

func testSimpleTransferVar() {
  var k = NonSendableKlass()
  k = NonSendableKlass()
  transferArg(k) // expected-warning {{sending 'k' risks causing data races}}
  // expected-note @-1 {{'k' used after being passed as a 'sending' parameter}}
  useValue(k) // expected-note {{access can happen concurrently}}
}

func testSimpleTransferUseOfOtherParamNoError() {
  let k = NonSendableKlass()
  let k2 = NonSendableKlass()
  transferArgWithOtherParam(k, k2)
  useValue(k2)
}

func testSimpleTransferUseOfOtherParamNoError2() {
  let k = NonSendableKlass()
  let k2 = NonSendableKlass()
  transferArgWithOtherParam2(k, k2)
  useValue(k)
}

@MainActor func transferToMain2(_ x: sending NonSendableKlass, _ y: NonSendableKlass, _ z: NonSendableKlass) async {
}

// TODO: How to test this?
func testNonStrongTransferDoesntMerge() async {
}

//////////////////////////////////
// MARK: Transferring Parameter //
//////////////////////////////////

func testTransferringParameter_canTransfer(_ x: sending NonSendableKlass, _ y: NonSendableKlass) async {
  await transferToMain(x)
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending task-isolated 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and task-isolated uses}}
}

func testTransferringParameter_cannotTransferTwice(_ x: sending NonSendableKlass, _ y: NonSendableKlass) async {
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}

  // TODO: We should not error on this since we are sending to the same place.
  await transferToMain(x) // expected-note {{access can happen concurrently}}
}

func testTransferringParameter_cannotUseAfterTransfer(_ x: sending NonSendableKlass, _ y: NonSendableKlass) async {
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

actor MyActor {
  var field = NonSendableKlass()

  func canTransferWithTransferringMethodArg(_ x: sending NonSendableKlass, _ y: NonSendableKlass) async {
    await transferToMain(x)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
    // expected-note @-1 {{sending 'self'-isolated 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and 'self'-isolated uses}}
  }

  func getNormalErrorIfTransferTwice(_ x: sending NonSendableKlass) async {
    await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local actor-isolated uses}}
    await transferToMain(x) // expected-note {{access can happen concurrently}}
  }

  func getNormalErrorIfUseAfterTransfer(_ x: sending NonSendableKlass) async {
    await transferToMain(x)  // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local actor-isolated uses}}
    useValue(x) // expected-note {{access can happen concurrently}}
  }

  // After assigning into the actor, we can still use x in the actor as long as
  // we don't transfer it.
  func assignTransferringIntoActor(_ x: sending NonSendableKlass) async {
    field = x
    useValue(x)
  }

  // Once we assign into the actor, we cannot transfer further.
  func assignTransferringIntoActor2(_ x: sending NonSendableKlass) async {
    field = x
    await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'self'-isolated 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and 'self'-isolated uses}}
  }
}

@MainActor func canAssignTransferringIntoGlobalActor(_ x: sending NonSendableKlass) async {
  globalKlass = x
}

@MainActor func canAssignTransferringIntoGlobalActor2(_ x: sending NonSendableKlass) async {
  globalKlass = x
  // TODO: This is incorrect! sending should be independent of @MainActor.
  await transferToCustom(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending main actor-isolated 'x' to global actor 'CustomActor'-isolated global function 'transferToCustom' risks causing data races between global actor 'CustomActor'-isolated and main actor-isolated uses}}
}

@MainActor func canAssignTransferringIntoGlobalActor3(_ x: sending NonSendableKlass) async {
  await transferToCustom(globalKlass) // expected-warning {{sending value of non-Sendable type 'NonSendableKlass' risks causing data races}}
  // expected-note @-1 {{sending main actor-isolated value of non-Sendable type 'NonSendableKlass' to global actor 'CustomActor'-isolated global function 'transferToCustom' risks causing races in between main actor-isolated and global actor 'CustomActor'-isolated uses}}
}

func canTransferAssigningIntoLocal(_ x: sending NonSendableKlass) async {
  let _ = x
  await transferToMain(x)
}

func canTransferAssigningIntoLocal2(_ x: sending NonSendableKlass) async {
  let _ = x
  await transferToMain(x)
  // We do not error here since we just load the value and do not do anything
  // with it.
  //
  // TODO: We should change let _ = x so that it has a move_value '_' or
  // something like that. It will also help move checking as well.
  let _ = x
}

func canTransferAssigningIntoLocal2a(_ x: sending NonSendableKlass) async {
  let _ = x
  await transferToMain(x)
  // We do not error here since we just load the value and do not do anything
  // with it.
  //
  // TODO: We should change let _ = x so that it has a move_value '_' or
  // something like that. It will also help move checking as well.
  _ = x
}

func canTransferAssigningIntoLocal3(_ x: sending NonSendableKlass) async {
  let _ = x
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  let y = x // expected-note {{access can happen concurrently}}
  _ = y
}

//////////////////////////////////////
// MARK: Transferring is "var" like //
//////////////////////////////////////

// Assigning into a 'sending' parameter is a merge.
func assigningIsAMerge(_ x: sending NonSendableKlass) async {
  let y = NonSendableKlass()

  x = y

  // We can still transfer y since x is disconnected.
  await transferToMain(y)
}

func assigningIsAMergeError(_ x: sending NonSendableKlass) async {
  let y = NonSendableKlass()

  x = y

  // We can still transfer y since x is disconnected.
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}

  useValue(x) // expected-note {{access can happen concurrently}}
}

func assigningIsAMergeAny(_ x: sending Any) async {
  // Ok, this is disconnected.
  let y = getAny()

  x = y

  await transferToMain(y)
}

func assigningIsAMergeAnyError(_ x: sending Any) async {
  // Ok, this is disconnected.
  let y = getAny()

  x = y

  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}

  useValue(x) // expected-note {{access can happen concurrently}}
}

func canTransferAfterAssign(_ x: sending Any) async {
  // Ok, this is disconnected.
  let y = getAny()

  // y is transferred into x.
  await transferToMain(x)

  x = y

  useValue(x)
}

func canTransferAfterAssignButUseIsError(_ x: sending Any) async {
  // Ok, this is disconnected.
  let y = getAny()

  // y is transferred into x.
  x = y

  // TODO: This should refer to the sending parameter.
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}

  useValue(x) // expected-note {{access can happen concurrently}}
}

func assignToEntireValueEliminatesEarlierTransfer(_ x: sending Any) async {
  // Ok, this is disconnected.
  let y = getAny()

  useValue(x)

  // Transfer x
  await transferToMain(x)

  // y is transferred into x. This shouldn't error.
  x = y

  useValue(x)
}

func mergeDoesNotEliminateEarlierTransfer(_ x: sending NonSendableStruct) async {


  // Ok, this is disconnected.
  let y = NonSendableKlass()

  useValue(x)

  // Transfer x
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}

  // y is assigned into a field of x.
  x.first = y // expected-note {{access can happen concurrently}}

  useValue(x)
}

func mergeDoesNotEliminateEarlierTransfer2(_ x: sending NonSendableStruct) async {
  // Ok, this is disconnected.
  let y = NonSendableKlass()

  useValue(x)

  // Transfer x
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}

  x.first = y  // expected-note {{access can happen concurrently}}
}

func doubleArgument() async {
  let x = NonSendableKlass()
  twoTransferArg(x, x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{'x' used after being passed as a 'sending' parameter}}
  // expected-note @-2 {{access can happen concurrently}}
}

func testTransferSrc(_ x: sending NonSendableKlass) async {
  let y = NonSendableKlass()
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  x = y // expected-note {{access can happen concurrently}}
}

func testTransferOtherParam(_ x: sending NonSendableKlass, y: NonSendableKlass) async {
  x = y
}

func testTransferOtherParamTuple(_ x: sending NonSendableKlass, y: (NonSendableKlass, NonSendableKlass)) async {
  x = y.0
}

func fakeInitOutside(operation: sending @escaping () async -> ()) {}

func taskIsolatedOutsideError(_ x: @escaping @MainActor () async -> ()) {
  fakeInitOutside(operation: x) // okay; x is @Sendable
}

@MainActor func actorIsolatedOutsideError(_ x: @escaping @MainActor () async -> ()) {
  fakeInitOutside(operation: x) // okay; x is @Sendable
}

func taskIsolatedInsideError(_ x: @escaping @MainActor () async -> ()) {
  func fakeInit(operation: sending @escaping () async -> ()) {}

  fakeInit(operation: x)  // okay; x is @Sendable
}

@MainActor func actorIsolatedInsideError(_ x: @escaping @MainActor () async -> ()) {
  func fakeInit(operation: sending @escaping () async -> ()) {}

  // We do not get an error here since fakeInit is MainActor isolated since it
  // is defined within this function. As a result, we are sending a @MainActor
  // isolated thing to a MainActor-isolated thing.
  fakeInit(operation: x)
}

// Make sure we error here on only the second since x by being assigned a part
// of y becomes task-isolated
func testMergeWithTaskIsolated(_ x: sending NonSendableKlass, y: NonSendableKlass) async {
  await transferToMain(x)
  x = y
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending task-isolated 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and task-isolated uses}}
}

@MainActor func testMergeWithActorIsolated(_ x: sending NonSendableKlass, y: NonSendableKlass) async {
  x = y
  await transferToCustom(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending main actor-isolated 'x' to global actor 'CustomActor'-isolated global function 'transferToCustom' risks causing data races between global actor 'CustomActor'-isolated and main actor-isolated uses}}
}


@available(SwiftStdlib 5.1, *)
actor NonSendableInit {
  var first: NonSendableKlass
  var second: NonSendableKlass? = nil {
    @storageRestrictions(initializes: first)
    init(initialValue)  {
      transferArg(initialValue!) // expected-warning {{sending 'initialValue' risks causing data races}}
      // expected-note @-1 {{'self'-isolated 'initialValue' is passed as a 'sending' parameter}}
      first = initialValue!
    }

    get { fatalError() }
    set { fatalError() }
  }
}

func testNoCrashWhenSendingNoEscapeClosure() async {
  func test(_ x: sending () -> ()) async {}

  let c = NonSendableKlass()
  await test { print(c) }
}

///////////////////////////////
// MARK: InOut Sending Tests //
///////////////////////////////

func testInOutSendingReinit(_ x: inout sending NonSendableKlass) async {
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
} // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

func testInOutSendingReinit2(_ x: inout sending NonSendableKlass) async {
  await transferToMain(x)
  x = NonSendableKlass()
}

func testInOutSendingReinit3(_ x: inout sending NonSendableKlass) async throws {
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}

  try throwingFunction() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

  x = NonSendableKlass()
}

func testInOutSendingReinit4(_ x: inout sending NonSendableKlass) async throws {
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}

  do {
    try throwingFunction()
    x = NonSendableKlass()
  } catch {
    throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
  }

  x = NonSendableKlass()
}

func testInOutSendingReinit5(_ x: inout sending NonSendableKlass) async throws {
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}

  do {
    try throwingFunction()
  } catch {
    throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
  }

  x = NonSendableKlass()
}

func testInOutSendingReinit6(_ x: inout sending NonSendableKlass) async throws {
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}

  do {
    try throwingFunction()
  } catch {
    throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
  }
} // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

actor InOutSendingWrongIsolationActor {
  var ns = NonSendableKlass()
  func testWrongIsolation(_ x: inout sending NonSendableKlass) {
    x = ns
  } // expected-warning {{'inout sending' parameter 'x' cannot be 'self'-isolated at end of function}}
  // expected-note @-1 {{'self'-isolated 'x' risks causing races in between 'self'-isolated uses and caller uses since caller assumes value is not actor isolated}}

  func testWrongIsolation2(_ x: inout sending NonSendableKlass) {
    let z = ns
    x = z
  } // expected-warning {{'inout sending' parameter 'x' cannot be 'self'-isolated at end of function}}
  // expected-note @-1 {{'self'-isolated 'x' risks causing races in between 'self'-isolated uses and caller uses since caller assumes value is not actor isolated}}
}

@MainActor
func testWrongIsolationGlobalIsolation(_ x: inout sending NonSendableKlass) {
  x = globalKlass
} // expected-warning {{'inout sending' parameter 'x' cannot be main actor-isolated at end of function}}
// expected-note @-1 {{main actor-isolated 'x' risks causing races in between main actor-isolated uses and caller uses since caller assumes value is not actor isolated}}

//////////////////////////////////////
// MARK: Return Inout Sending Tests //
//////////////////////////////////////

func returnInOutSendingDirectly(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingRegionLet(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingRegionVar(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}


func returnInOutSendingViaHelper(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingHelperDirect(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingDirectlyIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  if getBool() {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  fatalError()
}

func returnInOutSendingRegionLetIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  } else {
    print(y)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingRegionVarIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  if getBool() {
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  fatalError()
}

func returnInOutSendingViaHelperIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  if getBool() {
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  } else {
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingHelperDirectIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  if getBool() {
    return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  } else {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingViaHelperVarIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  if getBool() {
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  } else {
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingDirectlyElse(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  if getBool() {
    print(x)
    fatalError()
  } else {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingRegionLetElse(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  if getBool() {
    print(y)
    fatalError()
  } else {
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingRegionVarElse(_ x: inout sending NonSendableKlass, z: NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  if getBool() {
    print(y)
    return z
  } else {
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingViaHelperElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
  let y = x
  if getBool() {
    print(y)
    return z
  } else {
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingHelperDirectElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
  if getBool() {
    print(x)
    return z
  } else {
    return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingViaHelperVarElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  if getBool() {
    print(y)
    return z
  } else {
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingDirectlyFor(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
  for _ in 0..<1 {
    if getBool() {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(x)
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingDirectlyFor2(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
  for _ in 0..<1 {
    if getBool() {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(x)
  return z
}

func returnInOutSendingRegionLetFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingRegionLetFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingRegionLetFor3(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return z
}

func returnInOutSendingRegionVarFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingRegionVarFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingRegionVarFor3(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return z
}

func returnInOutSendingViaHelperFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(y)
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingViaHelperFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(y)
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingHelperDirectFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  for _ in 0..<1 {
    if getBool() {
      return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingViaHelperVarFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  for _ in 0..<1 {
    if getBool() {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(y)
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingDirectlyGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  guard getBool() else {
    print(x)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingRegionLetGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  guard getBool() else {
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingRegionVarGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  guard getBool() else {
    print(y)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingViaHelperGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  guard getBool() else {
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingHelperDirectGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  guard getBool() else {
    print(x)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingViaHelperVarGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  guard getBool() else {
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingViaHelperVar(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingDirectly<T>(_ x: inout sending T) -> T {
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingRegionLet<T>(_ x: inout sending T) -> T {
  let y = x
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingRegionVar<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingViaHelper<T>(_ x: inout sending T) -> T {
  let y = x
  return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingHelperDirect<T>(_ x: inout sending T) -> T {
  return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingViaHelperVar<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingDirectlyIf<T>(_ x: inout sending T) -> T {
  if getBool() {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  fatalError()
}

func returnGenericInOutSendingRegionLetIf<T>(_ x: inout sending T) -> T {
  let y = x
  if getBool() {
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  } else {
    print(y)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingRegionVarIf<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  if getBool() {
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  fatalError()
}

func returnGenericInOutSendingViaHelperIf<T>(_ x: inout sending T) -> T {
  let y = x
  if getBool() {
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  } else {
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingHelperDirectIf<T>(_ x: inout sending T) -> T {
  if getBool() {
    return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  } else {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingViaHelperVarIf<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  if getBool() {
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  } else {
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingDirectlyElse<T>(_ x: inout sending T) -> T {
  if getBool() {
    print(x)
    fatalError()
  } else {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingRegionLetElse<T>(_ x: inout sending T) -> T {
  let y = x
  if getBool() {
    print(y)
    fatalError()
  } else {
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingRegionVarElse<T>(_ x: inout sending T, z: T) -> T {
  var y = x
  y = x
  if getBool() {
    print(y)
    return z
  } else {
      return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingViaHelperElse<T>(_ x: inout sending T, _ z: T) -> T {
  let y = x
  if getBool() {
    print(y)
    return z
  } else {
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingHelperDirectElse<T>(_ x: inout sending T, _ z: T) -> T {
  if getBool() {
    print(x)
    return z
  } else {
    return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingViaHelperVarElse<T>(_ x: inout sending T, _ z: T) -> T {
  var y = x
  y = x
  if getBool() {
    print(y)
    return z
  } else {
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingDirectlyFor<T>(_ x: inout sending T, _ z: T) -> T {
  for _ in 0..<1 {
    if getBool() {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(x)
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingDirectlyFor2<T>(_ x: inout sending T, _ z: T) -> T {
  for _ in 0..<1 {
    if getBool() {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(x)
  return z
}

func returnGenericInOutSendingRegionLetFor<T>(_ x: inout sending T) -> T {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingRegionLetFor2<T>(_ x: inout sending T) -> T {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingRegionLetFor3<T>(_ x: inout sending T, _ z: T) -> T {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return z
}

func returnGenericInOutSendingRegionVarFor<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingRegionVarFor2<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingRegionVarFor3<T>(_ x: inout sending T, _ z: T) -> T {
  var y = x
  y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return z
}

func returnGenericInOutSendingViaHelperFor<T>(_ x: inout sending T) -> T {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(y)
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingViaHelperFor2<T>(_ x: inout sending T) -> T {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(y)
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingHelperDirectFor<T>(_ x: inout sending T) -> T {
  for _ in 0..<1 {
    if getBool() {
      return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingViaHelperVarFor<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  for _ in 0..<1 {
    if getBool() {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(y)
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingDirectlyGuard<T>(_ x: inout sending T) -> T {
  guard getBool() else {
    print(x)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingRegionLetGuard<T>(_ x: inout sending T) -> T {
  let y = x
  guard getBool() else {
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingRegionVarGuard<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  guard getBool() else {
    print(y)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingViaHelperGuard<T>(_ x: inout sending T) -> T {
  let y = x
  guard getBool() else {
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingHelperDirectGuard<T>(_ x: inout sending T) -> T {
  guard getBool() else {
    print(x)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingViaHelperVarGuard<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  guard getBool() else {
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

///////////////////////////////////////////////////
// MARK: ReturnSendingInOutTestActor Actor Tests //
///////////////////////////////////////////////////

actor ReturnSendingInOutTestActor {

  func testInOutSendingReinit(_ x: inout sending NonSendableKlass) async {
    await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local actor-isolated uses}}
  } // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

  func testInOutSendingReinit2(_ x: inout sending NonSendableKlass) async {
    await transferToMain(x)
    x = NonSendableKlass()
  }

  func testInOutSendingReinit3(_ x: inout sending NonSendableKlass) async throws {
    await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local actor-isolated uses}}


    try throwingFunction() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

    x = NonSendableKlass()
  }

  func testInOutSendingReinit4(_ x: inout sending NonSendableKlass) async throws {
    await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local actor-isolated uses}}


    do {
      try throwingFunction()
      x = NonSendableKlass()
    } catch {
      throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
    }

    x = NonSendableKlass()
  }

  func testInOutSendingReinit5(_ x: inout sending NonSendableKlass) async throws {
    await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local actor-isolated uses}}


    do {
      try throwingFunction()
    } catch {
      throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
    }

    x = NonSendableKlass()
  }

  func testInOutSendingReinit6(_ x: inout sending NonSendableKlass) async throws {
    await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local actor-isolated uses}}


    do {
      try throwingFunction()
    } catch {
      throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
    }
  } // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

  func returnInOutSendingDirectly(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingRegionLet(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingRegionVar(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingViaHelper(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingHelperDirect(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingDirectlyIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    if getBool() {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    fatalError()
  }

  func returnInOutSendingRegionLetIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    } else {
      print(y)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingRegionVarIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    fatalError()
  }

  func returnInOutSendingViaHelperIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    if getBool() {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingHelperDirectIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    if getBool() {
      return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    } else {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingViaHelperVarIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    if getBool() {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    } else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingDirectlyElse(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    if getBool() {
      print(x)
      fatalError()
    } else {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingRegionLetElse(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    if getBool() {
      print(y)
      fatalError()
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingRegionVarElse(_ x: inout sending NonSendableKlass, z: NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    if getBool() {
      print(y)
      return z
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingViaHelperElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    let y = x
    if getBool() {
      print(y)
      return z
    } else {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingHelperDirectElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    if getBool() {
      print(x)
      return z
    } else {
      return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingViaHelperVarElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    if getBool() {
      print(y)
      return z
    } else {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingDirectlyFor(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    for _ in 0..<1 {
      if getBool() {
        return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
        // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(x)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingDirectlyFor2(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    for _ in 0..<1 {
      if getBool() {
        return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
        // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(x)
    return z
  }

  func returnInOutSendingRegionLetFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingRegionLetFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingRegionLetFor3(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return z
  }

  func returnInOutSendingRegionVarFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingRegionVarFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingRegionVarFor3(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return z
  }

  func returnInOutSendingViaHelperFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(y)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingViaHelperFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingHelperDirectFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    for _ in 0..<1 {
      if getBool() {
        return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingViaHelperVarFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingDirectlyGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    guard getBool() else {
      print(x)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingRegionLetGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingRegionVarGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    guard getBool() else {
      print(y)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingViaHelperGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingHelperDirectGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    guard getBool() else {
      print(x)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingViaHelperVarGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingViaHelperVar(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingDirectly<T>(_ x: inout sending T) -> T {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingRegionLet<T>(_ x: inout sending T) -> T {
    let y = x
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingRegionVar<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingViaHelper<T>(_ x: inout sending T) -> T {
    let y = x
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingHelperDirect<T>(_ x: inout sending T) -> T {
    return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingViaHelperVar<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingDirectlyIf<T>(_ x: inout sending T) -> T {
    if getBool() {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    fatalError()
  }

  func returnGenericInOutSendingRegionLetIf<T>(_ x: inout sending T) -> T {
    let y = x
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    } else {
      print(y)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingRegionVarIf<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    fatalError()
  }

  func returnGenericInOutSendingViaHelperIf<T>(_ x: inout sending T) -> T {
    let y = x
    if getBool() {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingHelperDirectIf<T>(_ x: inout sending T) -> T {
    if getBool() {
      return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    } else {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingViaHelperVarIf<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    if getBool() {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    } else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingDirectlyElse<T>(_ x: inout sending T) -> T {
    if getBool() {
      print(x)
      fatalError()
    } else {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingRegionLetElse<T>(_ x: inout sending T) -> T {
    let y = x
    if getBool() {
      print(y)
      fatalError()
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingRegionVarElse<T>(_ x: inout sending T, z: T) -> T {
    var y = x
    y = x
    if getBool() {
      print(y)
      return z
    } else {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingViaHelperElse<T>(_ x: inout sending T, _ z: T) -> T {
    let y = x
    if getBool() {
      print(y)
      return z
    } else {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingHelperDirectElse<T>(_ x: inout sending T, _ z: T) -> T {
    if getBool() {
      print(x)
      return z
    } else {
      return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingViaHelperVarElse<T>(_ x: inout sending T, _ z: T) -> T {
    var y = x
    y = x
    if getBool() {
      print(y)
      return z
    } else {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingDirectlyFor<T>(_ x: inout sending T, _ z: T) -> T {
    for _ in 0..<1 {
      if getBool() {
        return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
        // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(x)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingDirectlyFor2<T>(_ x: inout sending T, _ z: T) -> T {
    for _ in 0..<1 {
      if getBool() {
        return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
        // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(x)
    return z
  }

  func returnGenericInOutSendingRegionLetFor<T>(_ x: inout sending T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingRegionLetFor2<T>(_ x: inout sending T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingRegionLetFor3<T>(_ x: inout sending T, _ z: T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return z
  }

  func returnGenericInOutSendingRegionVarFor<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingRegionVarFor2<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingRegionVarFor3<T>(_ x: inout sending T, _ z: T) -> T {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return z
  }

  func returnGenericInOutSendingViaHelperFor<T>(_ x: inout sending T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(y)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingViaHelperFor2<T>(_ x: inout sending T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingHelperDirectFor<T>(_ x: inout sending T) -> T {
    for _ in 0..<1 {
      if getBool() {
        return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingViaHelperVarFor<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingDirectlyGuard<T>(_ x: inout sending T) -> T {
    guard getBool() else {
      print(x)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingRegionLetGuard<T>(_ x: inout sending T) -> T {
    let y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingRegionVarGuard<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    guard getBool() else {
      print(y)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingViaHelperGuard<T>(_ x: inout sending T) -> T {
    let y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingHelperDirectGuard<T>(_ x: inout sending T) -> T {
    guard getBool() else {
      print(x)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingViaHelperVarGuard<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }
}

////////////////////////////////////////////////////////////////
// MARK: ReturnSendingInOutTestGlobalActorIsolatedClass Tests //
////////////////////////////////////////////////////////////////

@MainActor
class ReturnSendingInOutTestGlobalActorIsolatedClass {

  func testInOutSendingReinit(_ x: inout sending NonSendableKlass) async {
    await transferToCustom(x) // expected-warning {{sending 'x' risks causing data races}}
  } // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
    // expected-note @-2 {{sending 'x' to global actor 'CustomActor'-isolated global function 'transferToCustom' risks causing data races between global actor 'CustomActor'-isolated and local main actor-isolated uses}}

  func testInOutSendingReinit2(_ x: inout sending NonSendableKlass) async {
    await transferToCustom(x)
    x = NonSendableKlass()
  }

  func testInOutSendingReinit3(_ x: inout sending NonSendableKlass) async throws {
    await transferToCustom(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to global actor 'CustomActor'-isolated global function 'transferToCustom' risks causing data races between global actor 'CustomActor'-isolated and local main actor-isolated uses}}

    try throwingFunction() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

    x = NonSendableKlass()
  }

  func testInOutSendingReinit4(_ x: inout sending NonSendableKlass) async throws {
    await transferToCustom(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to global actor 'CustomActor'-isolated global function 'transferToCustom' risks causing data races between global actor 'CustomActor'-isolated and local main actor-isolated uses}}

    do {
      try throwingFunction()
      x = NonSendableKlass()
    } catch {
      throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
    }

    x = NonSendableKlass()
  }

  func testInOutSendingReinit5(_ x: inout sending NonSendableKlass) async throws {
    await transferToCustom(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to global actor 'CustomActor'-isolated global function 'transferToCustom' risks causing data races between global actor 'CustomActor'-isolated and local main actor-isolated uses}}

    do {
      try throwingFunction()
    } catch {
      throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
    }

    x = NonSendableKlass()
  }

  func testInOutSendingReinit6(_ x: inout sending NonSendableKlass) async throws {
    await transferToCustom(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to global actor 'CustomActor'-isolated global function 'transferToCustom' risks causing data races between global actor 'CustomActor'-isolated and local main actor-isolated uses}}

    do {
      try throwingFunction()
    } catch {
      throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
    }
  } // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

  func returnInOutSendingDirectly(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingRegionLet(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingRegionVar(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingViaHelper(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingHelperDirect(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingDirectlyIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    if getBool() {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    fatalError()
  }

  func returnInOutSendingRegionLetIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    } else {
      print(y)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingRegionVarIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    fatalError()
  }

  func returnInOutSendingViaHelperIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    if getBool() {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingHelperDirectIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    if getBool() {
      return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    } else {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingViaHelperVarIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    if getBool() {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    } else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingDirectlyElse(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    if getBool() {
      print(x)
      fatalError()
    } else {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingRegionLetElse(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    if getBool() {
      print(y)
      fatalError()
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingRegionVarElse(_ x: inout sending NonSendableKlass, z: NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    if getBool() {
      print(y)
      return z
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingViaHelperElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    let y = x
    if getBool() {
      print(y)
      return z
    } else {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingHelperDirectElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    if getBool() {
      print(x)
      return z
    } else {
      return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingViaHelperVarElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    if getBool() {
      print(y)
      return z
    } else {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingDirectlyFor(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    for _ in 0..<1 {
      if getBool() {
        return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
        // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(x)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingDirectlyFor2(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    for _ in 0..<1 {
      if getBool() {
        return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
        // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(x)
    return z
  }

  func returnInOutSendingRegionLetFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingRegionLetFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingRegionLetFor3(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return z
  }

  func returnInOutSendingRegionVarFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingRegionVarFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingRegionVarFor3(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return z
  }

  func returnInOutSendingViaHelperFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(y)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingViaHelperFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingHelperDirectFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    for _ in 0..<1 {
      if getBool() {
        return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingViaHelperVarFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingDirectlyGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    guard getBool() else {
      print(x)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingRegionLetGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingRegionVarGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    guard getBool() else {
      print(y)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingViaHelperGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingHelperDirectGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    guard getBool() else {
      print(x)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingViaHelperVarGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingViaHelperVar(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingDirectly<T>(_ x: inout sending T) -> T {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingRegionLet<T>(_ x: inout sending T) -> T {
    let y = x
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingRegionVar<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingViaHelper<T>(_ x: inout sending T) -> T {
    let y = x
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingHelperDirect<T>(_ x: inout sending T) -> T {
    return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingViaHelperVar<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingDirectlyIf<T>(_ x: inout sending T) -> T {
    if getBool() {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    fatalError()
  }

  func returnGenericInOutSendingRegionLetIf<T>(_ x: inout sending T) -> T {
    let y = x
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    } else {
      print(y)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingRegionVarIf<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    fatalError()
  }

  func returnGenericInOutSendingViaHelperIf<T>(_ x: inout sending T) -> T {
    let y = x
    if getBool() {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingHelperDirectIf<T>(_ x: inout sending T) -> T {
    if getBool() {
      return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    } else {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingViaHelperVarIf<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    if getBool() {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    } else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingDirectlyElse<T>(_ x: inout sending T) -> T {
    if getBool() {
      print(x)
      fatalError()
    } else {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingRegionLetElse<T>(_ x: inout sending T) -> T {
    let y = x
    if getBool() {
      print(y)
      fatalError()
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingRegionVarElse<T>(_ x: inout sending T, z: T) -> T {
    var y = x
    y = x
    if getBool() {
      print(y)
      return z
    } else {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingViaHelperElse<T>(_ x: inout sending T, _ z: T) -> T {
    let y = x
    if getBool() {
      print(y)
      return z
    } else {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingHelperDirectElse<T>(_ x: inout sending T, _ z: T) -> T {
    if getBool() {
      print(x)
      return z
    } else {
      return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingViaHelperVarElse<T>(_ x: inout sending T, _ z: T) -> T {
    var y = x
    y = x
    if getBool() {
      print(y)
      return z
    } else {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingDirectlyFor<T>(_ x: inout sending T, _ z: T) -> T {
    for _ in 0..<1 {
      if getBool() {
        return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
        // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(x)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingDirectlyFor2<T>(_ x: inout sending T, _ z: T) -> T {
    for _ in 0..<1 {
      if getBool() {
        return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
        // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(x)
    return z
  }

  func returnGenericInOutSendingRegionLetFor<T>(_ x: inout sending T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingRegionLetFor2<T>(_ x: inout sending T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingRegionLetFor3<T>(_ x: inout sending T, _ z: T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return z
  }

  func returnGenericInOutSendingRegionVarFor<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingRegionVarFor2<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingRegionVarFor3<T>(_ x: inout sending T, _ z: T) -> T {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return z
  }

  func returnGenericInOutSendingViaHelperFor<T>(_ x: inout sending T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(y)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingViaHelperFor2<T>(_ x: inout sending T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingHelperDirectFor<T>(_ x: inout sending T) -> T {
    for _ in 0..<1 {
      if getBool() {
        return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingViaHelperVarFor<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingDirectlyGuard<T>(_ x: inout sending T) -> T {
    guard getBool() else {
      print(x)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingRegionLetGuard<T>(_ x: inout sending T) -> T {
    let y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingRegionVarGuard<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    guard getBool() else {
      print(y)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingViaHelperGuard<T>(_ x: inout sending T) -> T {
    let y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingHelperDirectGuard<T>(_ x: inout sending T) -> T {
    guard getBool() else {
      print(x)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingViaHelperVarGuard<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }
}

//////////////////////
// MARK: Misc Tests //
//////////////////////

func taskIsolatedCaptureInSendingClosureLiteral(_ x: NonSendableKlass) {
  Task { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    print(x) // expected-note {{closure captures 'x' which is accessible to code in the current task}}
  }

  Task { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    {
      print(x) // expected-note {{closure captures 'x' which is accessible to code in the current task}}
    }()
  }

  Task { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    { // expected-note {{closure captures 'x' which is accessible to code in the current task}}
      print($0)
    }(x)
  }

  takeClosure { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    print(x) // expected-note {{closure captures 'x' which is accessible to code in the current task}}
  }

  takeClosureAndParam(NonSendableKlass()) { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    print(x) // expected-note {{closure captures 'x' which is accessible to code in the current task}}
  }

  let y = (x, x)
  Task { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    print(y) // expected-note {{closure captures 'y' which is accessible to code in the current task}}
  }

  let z = (x, y)
  Task { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    print(y, z) // expected-note @:11 {{closure captures non-Sendable 'y'}}
    // expected-note @-1:14 {{closure captures non-Sendable 'z'}}
  }
}

extension MyActor {
  func actorIsolatedCaptureInSendingClosureLiteral(_ x: NonSendableKlass) {
    Task { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      print(x) // expected-note {{closure captures 'self'-isolated 'x'}}
    }

    takeClosure { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      print(x) // expected-note {{closure captures 'self'-isolated 'x'}}
    }

    takeClosureAndParam(NonSendableKlass()) { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      print(x) // expected-note {{closure captures 'self'-isolated 'x'}}
    }

    let y = (x, x)
    Task { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      print(y) // expected-note {{closure captures 'y' which is accessible to 'self'-isolated code}}
    }

    let z = (x, y)
    Task { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between 'self'-isolated code and concurrent execution of the closure}}
      print(y, z) // expected-note @:13 {{closure captures non-Sendable 'y'}}
      // expected-note @-1:16 {{closure captures non-Sendable 'z'}}
    }
  }
}

// We would normally not error here since transferArg is nonisolated and c is
// disconnected. Since c is passed as sending, we shouldn't squelch this.
func disconnectedPassedSendingToNonIsolatedCallee(
) async -> Void {
    let c = NonSendableKlass()
    transferArg(c) // expected-warning {{sending 'c' risks causing data races}}
    // expected-note @-1 {{'c' used after being passed as a 'sending' parameter}}
    c.use() // expected-note {{access can happen concurrently}}
}

// We would normally not error here since transferArg is nonisolated and c is
// disconnected. Since c is passed as sending, we shouldn't squelch this.
func disconnectedPassedSendingToAsyncNonIsolatedCallee(
) async -> Void {
    let c = NonSendableKlass()
    await transferArgAsync(c) // expected-warning {{sending 'c' risks causing data races}}
    // expected-note @-1 {{'c' used after being passed as a 'sending' parameter}}
    c.use()  // expected-note {{access can happen concurrently}}
}

// We would normally not error here since transferArg is nonisolated and c is
// disconnected. Since c is passed as sending, we shouldn't squelch this.
func disconnectedPassedSendingToNonIsolatedCalleeIsolatedParam2(
    isolation: isolated (any Actor)? = nil
) async -> Void {
    let c = NonSendableKlass()
    transferArg(c) // expected-warning {{sending 'c' risks causing data races}}
    // expected-note @-1 {{'c' used after being passed as a 'sending' parameter}}
    c.use() // expected-note {{access can happen concurrently}}
}

// We would normally not error here since transferArg is nonisolated and c is
// disconnected. Since c is passed as sending, we shouldn't squelch this.
func disconnectedPassedSendingToAsyncNonIsolatedCalleeIsolatedParam2(
  isolation: isolated (any Actor)? = nil
) async -> Void {
    let c = NonSendableKlass()
    await transferArgAsync(c) // expected-warning {{sending 'c' risks causing data races}}
    // expected-note @-1 {{'c' used after being passed as a 'sending' parameter}}
    c.use() // expected-note {{access can happen concurrently}}
}

// We would normally not error here since transferArg is nonisolated and c is
// disconnected. Since c is passed as sending, we shouldn't squelch this.
func disconnectedPassedSendingToNonIsolatedCalleeIsolatedParam3(
    isolation: isolated (any Actor)? = nil
) -> Void {
    let c = NonSendableKlass()
    transferArg(c) // expected-warning {{sending 'c' risks causing data races}}
    // expected-note @-1 {{'c' used after being passed as a 'sending' parameter}}
    c.use() // expected-note {{access can happen concurrently}}
}

// In all of the below, we don't know that 'a' is the same isolation as the
// closure isolation.
func testNonSendableCaptures(ns: NonSendableKlass, a: isolated MyActor) {
  Task {
    _ = a
    _ = ns
  }

  Task { [a] in // expected-warning {{passing closure as a 'sending' parameter risks causing data races between 'a'-isolated code and concurrent execution of the closure}}
    _ = a
    _ = ns // expected-note {{closure captures 'a'-isolated 'ns'}}
  }

  Task {
    let _ = a
    let _ = ns
  }

  Task { [a] in // expected-warning {{passing closure as a 'sending' parameter risks causing data races between 'a'-isolated code and concurrent execution of the closure}}
    let _ = a
    let _ = ns // expected-note {{closure captures 'a'-isolated 'ns'}}
  }

  Task { [a] in // expected-warning {{passing closure as a 'sending' parameter risks causing data races between 'a'-isolated code and concurrent execution of the closure}}
    let (_, _) = (a, ns) // expected-note {{closure captures 'a'-isolated 'ns'}}
    let _ = ns
  }
}
