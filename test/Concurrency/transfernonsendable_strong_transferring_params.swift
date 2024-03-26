// RUN: %target-swift-frontend -emit-sil -parse-as-library -disable-availability-checking -strict-concurrency=complete -enable-experimental-feature TransferringArgsAndResults -verify -enable-upcoming-feature RegionBasedIsolation %s -o /dev/null

// REQUIRES: asserts

////////////////////////
// MARK: Declarations //
////////////////////////

class Klass {}

struct NonSendableStruct {
  var first = Klass()
  var second = Klass()
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

func transferArg(_ x: transferring Klass) {
}

func transferArgWithOtherParam(_ x: transferring Klass, _ y: Klass) {
}

func transferArgWithOtherParam2(_ x: Klass, _ y: transferring Klass) {
}

func twoTransferArg(_ x: transferring Klass, _ y: transferring Klass) {}

@MainActor var globalKlass = Klass()

/////////////////
// MARK: Tests //
/////////////////

func testSimpleTransferLet() {
  let k = Klass()
  transferArg(k) // expected-warning {{transferring 'k' may cause a race}}
  // expected-note @-1 {{'k' used after being passed as a transferring parameter}}
  useValue(k) // expected-note {{use here could race}}
}

func testSimpleTransferVar() {
  var k = Klass()
  k = Klass()
  transferArg(k) // expected-warning {{transferring 'k' may cause a race}}
  // expected-note @-1 {{'k' used after being passed as a transferring parameter}}
  useValue(k) // expected-note {{use here could race}}
}

func testSimpleTransferUseOfOtherParamNoError() {
  let k = Klass()
  let k2 = Klass()
  transferArgWithOtherParam(k, k2)
  useValue(k2)
}

func testSimpleTransferUseOfOtherParamNoError2() {
  let k = Klass()
  let k2 = Klass()
  transferArgWithOtherParam2(k, k2)
  useValue(k)
}

@MainActor func transferToMain2(_ x: transferring Klass, _ y: Klass, _ z: Klass) async {

}

// TODO: How to test this?
func testNonStrongTransferDoesntMerge() async {
}

//////////////////////////////////
// MARK: Transferring Parameter //
//////////////////////////////////

func testTransferringParameter_canTransfer(_ x: transferring Klass, _ y: Klass) async {
  await transferToMain(x)
  await transferToMain(y) // expected-warning {{transferring 'y' may cause a race}}
  // expected-note @-1 {{transferring task-isolated 'y' to main actor-isolated callee could cause races between main actor-isolated and task-isolated uses}}
}

func testTransferringParameter_cannotTransferTwice(_ x: transferring Klass, _ y: Klass) async {
  await transferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{transferring disconnected 'x' to main actor-isolated callee could cause races in between callee main actor-isolated and local nonisolated uses}}

  // TODO: We should not error on this since we are transferring to the same place.
  await transferToMain(x) // expected-note {{use here could race}}
}

func testTransferringParameter_cannotUseAfterTransfer(_ x: transferring Klass, _ y: Klass) async {
  await transferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{transferring disconnected 'x' to main actor-isolated callee could cause races in between callee main actor-isolated and local nonisolated uses}}
  useValue(x) // expected-note {{use here could race}}
}

actor MyActor {
  var field = Klass()

  func canTransferWithTransferringMethodArg(_ x: transferring Klass, _ y: Klass) async {
    await transferToMain(x)
    await transferToMain(y) // expected-warning {{transferring 'y' may cause a race}}
    // expected-note @-1 {{transferring actor-isolated 'y' to main actor-isolated callee could cause races between main actor-isolated and actor-isolated uses}}
  }

  func getNormalErrorIfTransferTwice(_ x: transferring Klass) async {
    await transferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
    // expected-note @-1 {{transferring disconnected 'x' to main actor-isolated callee could cause races in between callee main actor-isolated and local actor-isolated uses}}
    await transferToMain(x) // expected-note {{use here could race}}
  }

  func getNormalErrorIfUseAfterTransfer(_ x: transferring Klass) async {
    await transferToMain(x)  // expected-warning {{transferring 'x' may cause a race}}
    // expected-note @-1 {{transferring disconnected 'x' to main actor-isolated callee could cause races in between callee main actor-isolated and local actor-isolated uses}}
    useValue(x) // expected-note {{use here could race}}
  }

  // After assigning into the actor, we can still use x in the actor as long as
  // we don't transfer it.
  func assignTransferringIntoActor(_ x: transferring Klass) async {
    field = x
    useValue(x)
  }

  // Once we assign into the actor, we cannot transfer further.
  func assignTransferringIntoActor2(_ x: transferring Klass) async {
    field = x
    await transferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
    // expected-note @-1 {{transferring actor-isolated 'x' to main actor-isolated callee could cause races between main actor-isolated and actor-isolated uses}}
  }
}

@MainActor func canAssignTransferringIntoGlobalActor(_ x: transferring Klass) async {
  globalKlass = x
}

@MainActor func canAssignTransferringIntoGlobalActor2(_ x: transferring Klass) async {
  globalKlass = x
  // TODO: This is incorrect! transferring should be independent of @MainActor.
  await transferToCustom(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{transferring main actor-isolated 'x' to global actor 'CustomActor'-isolated callee could cause races between global actor 'CustomActor'-isolated and main actor-isolated uses}}
}

@MainActor func canAssignTransferringIntoGlobalActor3(_ x: transferring Klass) async {
  await transferToCustom(globalKlass) // expected-warning {{main actor-isolated value of type 'Klass' transferred to global actor 'CustomActor'-isolated context}}
}

func canTransferAssigningIntoLocal(_ x: transferring Klass) async {
  let _ = x
  await transferToMain(x)
}

func canTransferAssigningIntoLocal2(_ x: transferring Klass) async {

  let _ = x
  await transferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{transferring disconnected 'x' to main actor-isolated callee could cause races in between callee main actor-isolated and local nonisolated uses}}
  let _ = x // expected-note {{use here could race}}
}


//////////////////////////////////////
// MARK: Transferring is "var" like //
//////////////////////////////////////

// Assigning into a transferring parameter is a merge.
func assigningIsAMerge(_ x: transferring Klass) async {
  let y = Klass()

  x = y

  // We can still transfer y since x is disconnected.
  await transferToMain(y)
}

func assigningIsAMergeError(_ x: transferring Klass) async {
  let y = Klass()

  x = y

  // We can still transfer y since x is disconnected.
  await transferToMain(y) // expected-warning {{transferring 'y' may cause a race}}
  // expected-note @-1 {{transferring disconnected 'y' to main actor-isolated callee could cause races in between callee main actor-isolated and local nonisolated uses}}

  useValue(x) // expected-note {{use here could race}}
}

func assigningIsAMergeAny(_ x: transferring Any) async {
  // Ok, this is disconnected.
  let y = getAny()

  x = y

  await transferToMain(y)
}

func assigningIsAMergeAnyError(_ x: transferring Any) async {
  // Ok, this is disconnected.
  let y = getAny()

  x = y

  await transferToMain(y) // expected-warning {{transferring 'y' may cause a race}}
  // expected-note @-1 {{transferring disconnected 'y' to main actor-isolated callee could cause races in between callee main actor-isolated and local nonisolated uses}}

  useValue(x) // expected-note {{use here could race}}
}

func canTransferAfterAssign(_ x: transferring Any) async {
  // Ok, this is disconnected.
  let y = getAny()

  // y is transferred into x.
  await transferToMain(x)

  x = y

  useValue(x)
}

func canTransferAfterAssignButUseIsError(_ x: transferring Any) async {
  // Ok, this is disconnected.
  let y = getAny()

  // y is transferred into x.
  x = y

  // TODO: This should refer to the transferring parameter.
  await transferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{transferring disconnected 'x' to main actor-isolated callee could cause races in between callee main actor-isolated and local nonisolated uses}}

  useValue(x) // expected-note {{use here could race}}
}

func assignToEntireValueEliminatesEarlierTransfer(_ x: transferring Any) async {
  // Ok, this is disconnected.
  let y = getAny()

  useValue(x)

  // Transfer x
  await transferToMain(x)

  // y is transferred into x. This shouldn't error.
  x = y

  useValue(x)
}

func mergeDoesNotEliminateEarlierTransfer(_ x: transferring NonSendableStruct) async {


  // Ok, this is disconnected.
  let y = Klass()

  useValue(x)

  // Transfer x
  await transferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{transferring disconnected 'x' to main actor-isolated callee could cause races in between callee main actor-isolated and local nonisolated uses}}

  // y is assigned into a field of x.
  x.first = y // expected-note {{use here could race}}

  useValue(x)
}

func mergeDoesNotEliminateEarlierTransfer2(_ x: transferring NonSendableStruct) async {
  // Ok, this is disconnected.
  let y = Klass()

  useValue(x)

  // Transfer x
  await transferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{transferring disconnected 'x' to main actor-isolated callee could cause races in between callee main actor-isolated and local nonisolated uses}}

  x.first = y  // expected-note {{use here could race}}
}

func doubleArgument() async {
  let x = Klass()
  twoTransferArg(x, x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{'x' used after being passed as a transferring parameter}}
  // expected-note @-2 {{use here could race}}
}

func testTransferSrc(_ x: transferring Klass) async {
  let y = Klass()
  await transferToMain(y) // expected-warning {{transferring 'y' may cause a race}}
  // expected-note @-1 {{transferring disconnected 'y' to main actor-isolated callee could cause races in between callee main actor-isolated and local nonisolated uses}}
  x = y // expected-note {{use here could race}}
}

func testTransferOtherParam(_ x: transferring Klass, y: Klass) async {
  x = y
}

func testTransferOtherParamTuple(_ x: transferring Klass, y: (Klass, Klass)) async {
  x = y.0
}

func taskIsolatedError(_ x: @escaping @MainActor () async -> ()) {
  func fakeInit(operation: transferring @escaping () async -> ()) {}

  fakeInit(operation: x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{task-isolated 'x' is passed as a transferring parameter; Uses in callee may race with later task-isolated uses}}
}

@MainActor func actorIsolatedError(_ x: @escaping @MainActor () async -> ()) {
  func fakeInit(operation: transferring @escaping () async -> ()) {}

  // TODO: This needs to say actor-isolated.
  fakeInit(operation: x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{main actor-isolated 'x' is passed as a transferring parameter; Uses in callee may race with later main actor-isolated uses}}
}

// Make sure we error here on only the second since x by being assigned a part
// of y becomes task-isolated
func testMergeWithTaskIsolated(_ x: transferring Klass, y: Klass) async {
  await transferToMain(x)
  x = y
  await transferToMain(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{transferring task-isolated 'x' to main actor-isolated callee could cause races between main actor-isolated and task-isolated uses}}
}

@MainActor func testMergeWithActorIsolated(_ x: transferring Klass, y: Klass) async {
  x = y
  await transferToCustom(x) // expected-warning {{transferring 'x' may cause a race}}
  // expected-note @-1 {{transferring main actor-isolated 'x' to global actor 'CustomActor'-isolated callee could cause races between global actor 'CustomActor'-isolated and main actor-isolated uses}}
}
