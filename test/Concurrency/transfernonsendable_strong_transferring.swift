// RUN: %target-swift-frontend -emit-sil -disable-availability-checking -enable-experimental-feature TransferringArgsAndResults -verify -enable-experimental-feature RegionBasedIsolation %s

// REQUIRES: asserts

////////////////////////
// MARK: Declarations //
////////////////////////

class Klass {}

struct NonSendableStruct {
  var first = Klass()
  var second = Klass()
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

@MainActor var globalKlass = Klass()

/////////////////
// MARK: Tests //
/////////////////

func testSimpleTransferLet() {
  let k = Klass()
  transferArg(k) // expected-warning {{binding of non-Sendable type 'Klass' accessed after being transferred; later accesses could result in races}}
  useValue(k) // expected-note {{access here could race}}
}

func testSimpleTransferVar() {
  var k = Klass()
  k = Klass()
  transferArg(k) // expected-warning {{binding of non-Sendable type 'Klass' accessed after being transferred; later accesses could result in races}}
  useValue(k) // expected-note {{access here could race}}
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
  await transferToMain(y) // expected-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
}

func testTransferringParameter_cannotTransferTwice(_ x: transferring Klass, _ y: Klass) async {
  await transferToMain(x) // expected-warning {{passing argument of non-sendable type 'Klass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  await transferToMain(x) // expected-note {{access here could race}}
}

func testTransferringParameter_cannotUseAfterTransfer(_ x: transferring Klass, _ y: Klass) async {
  await transferToMain(x) // expected-warning {{passing argument of non-sendable type 'Klass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  useValue(x) // expected-note {{access here could race}}
}

actor MyActor {
  var field = Klass()

  func canTransferWithTransferringMethodArg(_ x: transferring Klass, _ y: Klass) async {
    await transferToMain(x)
    await transferToMain(y) // expected-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
  }

  func getNormalErrorIfTransferTwice(_ x: transferring Klass) async {
    await transferToMain(x) // expected-warning {{passing argument of non-sendable type 'Klass' from actor-isolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    await transferToMain(x) // expected-note {{access here could race}}
  }

  func getNormalErrorIfUseAfterTransfer(_ x: transferring Klass) async {
    await transferToMain(x)  // expected-warning {{passing argument of non-sendable type 'Klass' from actor-isolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
    useValue(x) // expected-note {{access here could race}}
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
    await transferToMain(x) // expected-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
  }
}

@MainActor func canAssignTransferringIntoGlobalActor(_ x: transferring Klass) async {
  globalKlass = x
}

@MainActor func canAssignTransferringIntoGlobalActor2(_ x: transferring Klass) async {
  globalKlass = x
  await transferToCustom(x) // expected-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
}

@MainActor func canAssignTransferringIntoGlobalActor3(_ x: transferring Klass) async {
  await transferToCustom(globalKlass) // expected-warning {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
}

func canTransferAssigningIntoLocal(_ x: transferring Klass) async {
  let _ = x
  await transferToMain(x)
}

func canTransferAssigningIntoLocal2(_ x: transferring Klass) async {
  let _ = x
  await transferToMain(x) // expected-warning {{passing argument of non-sendable type 'Klass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  let _ = x // expected-note {{access here could race}}
}


//////////////////////////////////////
// MARK: Transferring is "var" like //
//////////////////////////////////////

// Assigning into a transferring parameter is a transfer!
func assigningIsATransfer(_ x: transferring Klass) async {
  // Ok, this is disconnected.
  let y = Klass()

  // y is transferred into x.
  x = y // expected-warning {{transferred value of non-Sendable type 'Klass' into transferring parameter; later accesses could result in races}}

  // Since y is now in x, we can't use y anymore so we emit an error.
  useValue(y) // expected-note {{access here could race}}
}

func assigningIsATransferNoError(_ x: transferring Klass) async {
  // Ok, this is disconnected.
  let y = Klass()

  // y is transferred into x.
  x = y

  useValue(x)
}

func assigningIsATransferAny(_ x: transferring Any) async {
  // Ok, this is disconnected.
  let y = getAny()

  // y is transferred into x.
  x = y // expected-warning {{transferred value of non-Sendable type 'Any' into transferring parameter; later accesses could result in races}}

  // Since y is now in x, we can't use y anymore so we emit an error.
  useValue(y) // expected-note {{access here could race}}
}

func canTransferAfterAssign(_ x: transferring Any) async {
  // Ok, this is disconnected.
  let y = getAny()

  // y is transferred into x.
  x = y

  await transferToMain(x)
}

func canTransferAfterAssignButUseIsError(_ x: transferring Any) async {
  // Ok, this is disconnected.
  let y = getAny()

  // y is transferred into x.
  x = y

  // TODO: Change this to refer to task context? Or to transferring parameter?
  await transferToMain(x) // expected-warning {{passing argument of non-sendable type 'Any' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}

  useValue(x) // expected-note {{access here could race}}
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
  await transferToMain(x) // expected-warning {{passing argument of non-sendable type 'NonSendableStruct' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}

  // y is assigned into a field of x, so we treat this like a merge.
  x.first = y

  useValue(x) // expected-note {{access here could race}}
}

func mergeDoesNotEliminateEarlierTransfer2(_ x: transferring NonSendableStruct) async {
  // Ok, this is disconnected.
  let y = Klass()

  useValue(x)

  // Transfer x
  await transferToMain(x) // expected-warning {{passing argument of non-sendable type 'NonSendableStruct' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}

  // y is assigned into a field of x, so we treat this like a merge.
  x.first = y // expected-warning {{transferred value of non-Sendable type 'Klass' into transferring parameter; later accesses could result in races}}

  useValue(x) // expected-note {{access here could race}}

  useValue(y) // expected-note {{access here could race}}
}
