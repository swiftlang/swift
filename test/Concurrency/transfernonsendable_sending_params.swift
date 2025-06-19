// RUN: %target-swift-frontend -emit-sil -parse-as-library -target %target-swift-5.1-abi-triple -strict-concurrency=complete -verify %s -o /dev/null -enable-upcoming-feature GlobalActorIsolatedTypesUsability

// REQUIRES: concurrency
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability

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
