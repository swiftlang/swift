// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -target %target-swift-5.1-abi-triple -verify -verify-additional-prefix tns-  %s -o /dev/null -enable-upcoming-feature GlobalActorIsolatedTypesUsability

// READ THIS: This test is intended to centralize all tests that use
// nonisolated(unsafe).

// REQUIRES: concurrency
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {
  var field: NonSendableKlass? = nil
}

struct NonSendableStruct {
  var field: NonSendableKlass? = nil
}

protocol ProvidesStaticValue {
  static var value: Self { get }
}

@MainActor func transferToMainIndirect<T>(_ t: T) async {}
@MainActor func transferToMainDirect(_ t: NonSendableKlass) async {}
@MainActor func transferToMainDirect(_ t: NonSendableStruct) async {}

@MainActor func transferToMainIndirectConsuming<T>(_ t: consuming T) async {}
@MainActor func transferToMainDirectConsuming(_ t: consuming NonSendableKlass) async {}

func useInOut<T>(_ t: inout T) {}

actor CustomActorInstance {}

@globalActor
struct CustomActor {
  static let shared = CustomActorInstance()
}

@CustomActor func transferToCustom<T>(_ t: T) async {}

/////////////////
// MARK: Tests //
/////////////////

// We should only squelch ns2. All other elements in the ns region should result
// in errors.
func transferLetNonTransferrableSquelched(_ ns: NonSendableKlass) async {
  nonisolated(unsafe) let ns2 = ns
  let ns3 = ns2
  let ns4 = ns

  await transferToMainDirect(ns)
  // expected-tns-warning @-1 {{sending 'ns' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  await transferToMainDirect(ns2)

  await transferToMainDirect(ns3)
  // expected-tns-warning @-1 {{sending 'ns3' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns3' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  await transferToMainDirect(ns4)
  // expected-tns-warning @-1 {{sending 'ns4' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns4' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  await transferToMainIndirect(ns)
  // expected-tns-warning @-1 {{sending 'ns' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  await transferToMainIndirect(ns2)

  await transferToMainIndirect(ns3)
  // expected-tns-warning @-1 {{sending 'ns3' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns3' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  await transferToMainIndirect(ns4)
  // expected-tns-warning @-1 {{sending 'ns4' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns4' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
}

func useAfterTransferLetSquelchedDirect() async {
  let ns = NonSendableKlass()
  nonisolated(unsafe) let ns2 = ns
  let ns3 = ns2
  let ns4 = ns

  await transferToMainDirect(ns)
  // expected-tns-warning @-1 {{sending 'ns' risks causing data races}}
  // expected-tns-note @-2 {{sending 'ns' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and local nonisolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  print(ns) // expected-tns-note {{access can happen concurrently}}

  await transferToMainDirect(ns2)
  print(ns2)

  await transferToMainDirect(ns3)
  // expected-tns-warning @-1 {{sending 'ns3' risks causing data races}}
  // expected-tns-note @-2 {{sending 'ns3' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and local nonisolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  print(ns3) // expected-tns-note {{access can happen concurrently}}

  await transferToMainDirect(ns4)
  // expected-tns-warning @-1 {{sending 'ns4' risks causing data races}}
  // expected-tns-note @-2 {{sending 'ns4' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and local nonisolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  print(ns4) // expected-tns-note {{access can happen concurrently}}
}

func useAfterTransferSquelchedIndirect() async {
  let ns = NonSendableKlass()
  nonisolated(unsafe) let ns2 = ns
  let ns3 = ns2
  let ns4 = ns

  await transferToMainIndirect(ns)
  // expected-tns-warning @-1 {{sending 'ns' risks causing data races}}
  // expected-tns-note @-2 {{sending 'ns' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and local nonisolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  print(ns) // expected-tns-note {{access can happen concurrently}}

  await transferToMainIndirect(ns2)
  print(ns2)

  await transferToMainIndirect(ns3)
  // expected-tns-warning @-1 {{sending 'ns3' risks causing data races}}
  // expected-tns-note @-2 {{sending 'ns3' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and local nonisolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  print(ns3) // expected-tns-note {{access can happen concurrently}}

  await transferToMainIndirect(ns4)
  // expected-tns-warning @-1 {{sending 'ns4' risks causing data races}}
  // expected-tns-note @-2 {{sending 'ns4' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and local nonisolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  print(ns4) // expected-tns-note {{access can happen concurrently}}
}

// We consider the klass field separate from the klass, so we get an error.
func transferNonTransferrableClassField(_ ns: NonSendableKlass) async {
  nonisolated(unsafe) let ns2 = ns

  await transferToMainDirect(ns2.field!)
  // expected-tns-warning @-1 {{sending 'ns2.field' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns2.field' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  await transferToMainIndirect(ns2.field!)
  // expected-tns-warning @-1 {{sending 'ns2.field' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns2.field' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
}

// We consider the klass field separate from the klass, so we get an error.
func transferNonTransferrableStructField(_ ns: NonSendableStruct) async {
  nonisolated(unsafe) let ns2 = ns

  await transferToMainDirect(ns2)
  await transferToMainIndirect(ns2)

  await transferToMainDirect(ns2.field!)
  // expected-tns-warning @-1 {{sending 'ns2.field' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns2.field' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  await transferToMainIndirect(ns2.field!)
  // expected-tns-warning @-1 {{sending 'ns2.field' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns2.field' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  await transferToMainIndirect(ns2.field)
  // expected-tns-warning @-1 {{sending 'ns2.field' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns2.field' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass?' into main actor-isolated context may introduce data races}}
}

// Make sure that we pattern match the consuming temporary correctly.
func testConsumingTransfer(_ ns: NonSendableKlass) async {
  nonisolated(unsafe) let ns2 = ns

  await transferToMainDirectConsuming(ns)
  // expected-tns-warning @-1 {{sending 'ns' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns' to main actor-isolated global function 'transferToMainDirectConsuming' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  await transferToMainIndirectConsuming(ns)
  // expected-tns-warning @-1 {{sending 'ns' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns' to main actor-isolated global function 'transferToMainIndirectConsuming' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  await transferToMainDirectConsuming(ns2)

  await transferToMainIndirectConsuming(ns2)
}

/////

func transferVarNonTransferrableSquelched(_ ns: NonSendableKlass) async {
  nonisolated(unsafe) var ns2 = NonSendableKlass()
  ns2 = ns
  let ns3 = ns2
  let ns4 = ns

  await transferToMainDirect(ns)
  // expected-tns-warning @-1 {{sending 'ns' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  await transferToMainDirect(ns2)

  await transferToMainDirect(ns3)
  // expected-tns-warning @-1 {{sending 'ns3' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns3' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  await transferToMainDirect(ns4)
  // expected-tns-warning @-1 {{sending 'ns4' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns4' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  await transferToMainIndirect(ns)
  // expected-tns-warning @-1 {{sending 'ns' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  await transferToMainIndirect(ns2)

  await transferToMainIndirect(ns3)
  // expected-tns-warning @-1 {{sending 'ns3' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns3' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  await transferToMainIndirect(ns4)
  // expected-tns-warning @-1 {{sending 'ns4' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns4' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
}

func useAfterTransferVarSquelchedDirect() async {
  let ns = NonSendableKlass()
  nonisolated(unsafe) var ns2 = NonSendableKlass()
  ns2 = ns
  let ns3 = ns2
  let ns4 = ns

  await transferToMainDirect(ns)
  // expected-tns-warning @-1 {{sending 'ns' risks causing data races}}
  // expected-tns-note @-2 {{sending 'ns' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and local nonisolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  print(ns) // expected-tns-note {{access can happen concurrently}}

  await transferToMainDirect(ns2)
  print(ns2)

  await transferToMainDirect(ns3)
  // expected-tns-warning @-1 {{sending 'ns3' risks causing data races}}
  // expected-tns-note @-2 {{sending 'ns3' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and local nonisolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  print(ns3) // expected-tns-note {{access can happen concurrently}}

  await transferToMainDirect(ns4)
  // expected-tns-warning @-1 {{sending 'ns4' risks causing data races}}
  // expected-tns-note @-2 {{sending 'ns4' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and local nonisolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  print(ns4) // expected-tns-note {{access can happen concurrently}}
}

//////////////////////////////
// MARK: Address Only Tests //
//////////////////////////////

// We should only squelch ns2. All other elements in the ns region should result
// in errors.
func transferLetNonTransferrableSquelchedAddressOnly<T>(_ ns: T) async { // expected-complete-note 3{{}}
  nonisolated(unsafe) let ns2 = ns
  let ns3 = ns2
  let ns4 = ns

  await transferToMainIndirect(ns)
  // expected-tns-warning @-1 {{sending 'ns' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'T' into main actor-isolated context may introduce data races}}

  await transferToMainIndirect(ns2)

  await transferToMainIndirect(ns3)
  // expected-tns-warning @-1 {{sending 'ns3' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns3' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'T' into main actor-isolated context may introduce data races}}

  await transferToMainIndirect(ns4)
  // expected-tns-warning @-1 {{sending 'ns4' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns4' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'T' into main actor-isolated context may introduce data races}}
}

func useAfterTransferLetSquelchedIndirectAddressOnly<T : ProvidesStaticValue>(_ meta: T.Type) async { // expected-complete-note 3{{}}
  let ns = T.value
  nonisolated(unsafe) let ns2 = ns
  let ns3 = ns2
  let ns4 = ns

  await transferToMainIndirect(ns)
  // expected-tns-warning @-1 {{sending 'ns' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'T' into main actor-isolated context may introduce data races}}
  print(ns)

  await transferToMainIndirect(ns2)
  print(ns2)

  await transferToMainIndirect(ns3)
  // expected-tns-warning @-1 {{sending 'ns3' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns3' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'T' into main actor-isolated context may introduce data races}}
  print(ns3)

  await transferToMainIndirect(ns4)
  // expected-tns-warning @-1 {{sending 'ns4' risks causing data races}}
  // expected-tns-note @-2 {{sending task-isolated 'ns4' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and task-isolated uses}}
  // expected-complete-warning @-3 {{passing argument of non-Sendable type 'T' into main actor-isolated context may introduce data races}}
  print(ns4)
}

////////////////////////
// MARK: Global Tests //
////////////////////////

struct Globals {
  static nonisolated(unsafe) let nonIsolatedUnsafeLetObject = NonSendableKlass()
  static nonisolated(unsafe) var nonIsolatedUnsafeVarObject = NonSendableKlass()
}

func testAccessStaticGlobals() async {
  await transferToMainDirect(Globals.nonIsolatedUnsafeLetObject)
  // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  await transferToMainIndirect(Globals.nonIsolatedUnsafeLetObject)
  // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  await transferToMainDirect(Globals.nonIsolatedUnsafeVarObject)
  // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  await transferToMainIndirect(Globals.nonIsolatedUnsafeVarObject)
  // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
}

nonisolated(unsafe) let globalNonIsolatedUnsafeLetObject = NonSendableKlass()
nonisolated(unsafe) var globalNonIsolatedUnsafeVarObject = NonSendableKlass()

func testPassGlobalToMainActorIsolatedFunction() async {
  await transferToMainDirect(globalNonIsolatedUnsafeLetObject)
  await transferToMainIndirect(globalNonIsolatedUnsafeLetObject)
  await transferToMainDirect(globalNonIsolatedUnsafeVarObject)
  await transferToMainIndirect(globalNonIsolatedUnsafeVarObject)
}

// We use this to force the modify in testPassGlobalToModify
nonisolated(unsafe)
var computedGlobalNonIsolatedUnsafeVarObject : NonSendableKlass {
  _read {
    yield globalNonIsolatedUnsafeVarObject
  }
  _modify {
    yield &globalNonIsolatedUnsafeVarObject
  }
}

func testPassGlobalToModify() async {
  useInOut(&computedGlobalNonIsolatedUnsafeVarObject)
}

///////////////////////
// MARK: Field Tests //
///////////////////////

actor MyActor {
  nonisolated(unsafe) let nonIsolatedUnsafeLetObject = NonSendableKlass()
  nonisolated(unsafe) var nonIsolatedUnsafeVarObject = NonSendableKlass()

  func test() async {
    await transferToMainDirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

    let x = nonIsolatedUnsafeLetObject
    await transferToMainDirect(x)
    // expected-tns-warning @-1 {{sending 'x' risks causing data races}}
    // expected-tns-note @-2 {{sending 'self'-isolated 'x' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and 'self'-isolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  }
}

final actor MyFinalActor {
  nonisolated(unsafe) let nonIsolatedUnsafeLetObject = NonSendableKlass()
  nonisolated(unsafe) var nonIsolatedUnsafeVarObject = NonSendableKlass()

  func test() async {
    await transferToMainDirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

    let x = nonIsolatedUnsafeLetObject
    await transferToMainDirect(x)
    // expected-tns-warning @-1 {{sending 'x' risks causing data races}}
    // expected-tns-note @-2 {{sending 'self'-isolated 'x' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and 'self'-isolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  }
}

final class FinalNonIsolatedUnsafeFieldKlassSendable: @unchecked Sendable {
  nonisolated(unsafe) let nonIsolatedUnsafeLetObject = NonSendableKlass()
  nonisolated(unsafe) var nonIsolatedUnsafeVarObject = NonSendableKlass()
  let nonIsolatedLetObject = NonSendableKlass()

  func test() async {
    await transferToMainDirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

    // 'x' is treated as disconnected since we treat unchecked fields as being
    // disconnected.
    //
    // TODO: Is this correct?
    let x = nonIsolatedLetObject
    await transferToMainDirect(x)
    // expected-tns-warning @-1 {{sending 'x' risks causing data races}}
    // expected-tns-note @-2 {{sending 'x' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and local nonisolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    print(x) // expected-tns-note {{access can happen concurrently}}
  }
}

class NonIsolatedUnsafeFieldKlassSendable: @unchecked Sendable {
  nonisolated(unsafe) let nonIsolatedUnsafeLetObject = NonSendableKlass()
  nonisolated(unsafe) var nonIsolatedUnsafeVarObject = NonSendableKlass()

  func test() async {
    await transferToMainDirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

    // 'x' is treated as disconnected since we treat unchecked fields as being
    // disconnected.
    //
    // TODO: Is this correct?
    let x = nonIsolatedUnsafeLetObject
    await transferToMainDirect(x)
    // expected-tns-warning @-1 {{sending 'x' risks causing data races}}
    // expected-tns-note @-2 {{sending 'x' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and local nonisolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    print(x) // expected-tns-note {{access can happen concurrently}}
  }
}

struct NonIsolatedUnsafeFieldStruct: Sendable {
  nonisolated(unsafe) let nonIsolatedUnsafeLetObject = NonSendableKlass()
  nonisolated(unsafe) var nonIsolatedUnsafeVarObject = NonSendableKlass()

  func test() async {
    await transferToMainDirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

    // 'x' is treated as disconnected since we treat unchecked fields as being
    // disconnected.
    //
    // TODO: Is this correct?
    let x = nonIsolatedUnsafeLetObject
    await transferToMainDirect(x)
    // expected-tns-warning @-1 {{sending 'x' risks causing data races}}
    // expected-tns-note @-2 {{sending 'x' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and local nonisolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    print(x) // expected-tns-note {{access can happen concurrently}}
  }
}

enum NonIsolatedUnsafeComputedEnum: Sendable {
  case first
  case second

  nonisolated(unsafe) var nonIsolatedUnsafeVarObject: NonSendableKlass { NonSendableKlass() }
  // expected-warning@-1{{'nonisolated(unsafe)' has no effect on property 'nonIsolatedUnsafeVarObject', consider using 'nonisolated'}}

  func test() async {
    await transferToMainDirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

    // 'x' is treated as disconnected since we treat unchecked fields as being
    // disconnected.
    //
    // TODO: Is this correct?
    let x = nonIsolatedUnsafeVarObject
    await transferToMainDirect(x)
    // expected-tns-warning @-1 {{sending 'x' risks causing data races}}
    // expected-tns-note @-2 {{sending 'x' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and local nonisolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    print(x) // expected-tns-note {{access can happen concurrently}}
  }
}

@CustomActor final class CustomActorFinalNonIsolatedUnsafeFieldKlass {
  nonisolated(unsafe) let nonIsolatedUnsafeLetObject = NonSendableKlass()
  nonisolated(unsafe) var nonIsolatedUnsafeVarObject = NonSendableKlass()
  let nonIsolatedLetObject = NonSendableKlass()

  func test() async {
    await transferToMainDirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

    // 'x' is treated as global actor 'CustomActor' isolated.
    let x = nonIsolatedLetObject
    await transferToMainDirect(x)
    // expected-tns-warning @-1 {{sending 'x' risks causing data races}}
    // expected-tns-note @-2 {{sending global actor 'CustomActor'-isolated 'x' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and global actor 'CustomActor'-isolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    print(x)
  }
}

@CustomActor class CustomActorNonIsolatedUnsafeFieldKlass {
  nonisolated(unsafe) let nonIsolatedUnsafeLetObject = NonSendableKlass()
  nonisolated(unsafe) var nonIsolatedUnsafeVarObject = NonSendableKlass()

  func test() async {
    await transferToMainDirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

    // x is treated as global actor 'CustomActor' isolated since the
    // nonisolated(unsafe) only applies to nonIsolatedUnsafeLetObject.
    let x = nonIsolatedUnsafeLetObject
    await transferToMainDirect(x)
    // expected-tns-warning @-1 {{sending 'x' risks causing data races}}
    // expected-tns-note @-2 {{sending global actor 'CustomActor'-isolated 'x' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and global actor 'CustomActor'-isolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    print(x)
  }
}

@CustomActor struct CustomActorNonIsolatedUnsafeFieldStruct {
  nonisolated(unsafe) let nonIsolatedUnsafeLetObject = NonSendableKlass()
  nonisolated(unsafe) var nonIsolatedUnsafeVarObject = NonSendableKlass()

  func test() async {
    await transferToMainDirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

    // 'x' is treated as global actor 'CustomActor'-isolated.
    let x = nonIsolatedUnsafeLetObject
    await transferToMainDirect(x)
    // expected-tns-warning @-1 {{sending 'x' risks causing data races}}
    // expected-tns-note @-2 {{sending global actor 'CustomActor'-isolated 'x' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and global actor 'CustomActor'-isolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    print(x)
  }
}

@CustomActor struct CustomActorNonIsolatedUnsafeFieldAddressOnlyStruct<T> {
  nonisolated(unsafe) let nonIsolatedUnsafeLetObject = NonSendableKlass()
  nonisolated(unsafe) var nonIsolatedUnsafeVarObject = NonSendableKlass()
  nonisolated(unsafe) var nonIsolatedUnsafeVarComputedObject: NonSendableKlass { NonSendableKlass() }
  // expected-warning@-1{{'nonisolated(unsafe)' has no effect on property 'nonIsolatedUnsafeVarComputedObject', consider using 'nonisolated'}}

  var t: T? = nil

  func test() async {
    await transferToMainDirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

    // 'x' is treated as global actor 'CustomActor'-isolated.
    let x = nonIsolatedUnsafeLetObject
    await transferToMainDirect(x)
    // expected-tns-warning @-1 {{sending 'x' risks causing data races}}
    // expected-tns-note @-2 {{sending global actor 'CustomActor'-isolated 'x' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and global actor 'CustomActor'-isolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

    let x2 = nonIsolatedUnsafeVarObject
    await transferToMainDirect(x2)
    // expected-tns-warning @-1 {{sending 'x2' risks causing data races}}
    // expected-tns-note @-2 {{sending global actor 'CustomActor'-isolated 'x2' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and global actor 'CustomActor'-isolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

    let x3 = nonIsolatedUnsafeVarComputedObject
    await transferToMainDirect(x3)
    // expected-tns-warning @-1 {{sending 'x3' risks causing data races}}
    // expected-tns-note @-2 {{sending global actor 'CustomActor'-isolated 'x3' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and global actor 'CustomActor'-isolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

    print(x)
  }
}

@CustomActor enum CustomActorNonIsolatedUnsafeComputedEnum {
  case first
  case second

  nonisolated(unsafe) var nonIsolatedUnsafeVarObject: NonSendableKlass { NonSendableKlass() }
  // expected-warning@-1{{'nonisolated(unsafe)' has no effect on property 'nonIsolatedUnsafeVarObject', consider using 'nonisolated'}}

  func test() async {
    await transferToMainDirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

    // 'x' is treated as global actor 'CustomActor' even though
    // nonIsolatedUnsafeVarObject is not.
    let x = nonIsolatedUnsafeVarObject
    await transferToMainDirect(x)
    // expected-tns-warning @-1 {{sending 'x' risks causing data races}}
    // expected-tns-note @-2 {{sending global actor 'CustomActor'-isolated 'x' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and global actor 'CustomActor'-isolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    print(x)
  }
}

struct NonIsolatedUnsafeFieldNonSendableStruct {
  nonisolated(unsafe) let nonIsolatedUnsafeLetObject = NonSendableKlass()
  nonisolated(unsafe) var nonIsolatedUnsafeVarObject = NonSendableKlass()
  nonisolated(unsafe) var nonIsolatedUnsafeVarComputedObject: NonSendableKlass { NonSendableKlass() }
  // expected-warning@-1{{'nonisolated(unsafe)' has no effect on property 'nonIsolatedUnsafeVarComputedObject', consider using 'nonisolated'}}

  let letObject = NonSendableKlass()
  var varObject = NonSendableKlass()

  // This is unsafe since self is not main actor isolated, so our values are
  // task isolated.
  func test() async {
    await transferToMainDirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(letObject)
    // expected-tns-warning @-1 {{sending 'self.letObject' risks causing data races}}
    // expected-tns-note @-2 {{sending task-isolated 'self.letObject' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(varObject)
    // expected-tns-warning @-1 {{sending 'self.varObject' risks causing data races}}
    // expected-tns-note @-2 {{sending task-isolated 'self.varObject' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  }

  // This is safe since self will become main actor isolated as a result of
  // test2 running.
  @MainActor func test2() async {
    await transferToMainDirect(nonIsolatedUnsafeLetObject)
    await transferToMainDirect(nonIsolatedUnsafeVarObject)
    await transferToMainDirect(letObject)
    await transferToMainDirect(varObject)
  }
}

final class FinalNonIsolatedUnsafeFieldKlass {
  nonisolated(unsafe) let nonIsolatedUnsafeLetObject = NonSendableKlass()
  nonisolated(unsafe) var nonIsolatedUnsafeVarObject = NonSendableKlass()
  let letObject = NonSendableKlass()
  var varObject = NonSendableKlass()

  // This is unsafe since self is not main actor isolated, so our values are
  // task isolated.
  func test() async {
    await transferToMainDirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(letObject)
    // expected-tns-warning @-1 {{sending 'self.letObject' risks causing data races}}
    // expected-tns-note @-2 {{sending task-isolated 'self.letObject' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(varObject)
    // expected-tns-warning @-1 {{sending 'self.varObject' risks causing data races}}
    // expected-tns-note @-2 {{sending task-isolated 'self.varObject' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  }

  // This is safe since self will become main actor isolated as a result of
  // test2 running.
  @MainActor func test2() async {
    await transferToMainDirect(nonIsolatedUnsafeLetObject)
    await transferToMainDirect(nonIsolatedUnsafeVarObject)
    await transferToMainDirect(letObject)
    await transferToMainDirect(varObject)
  }
}

class NonIsolatedUnsafeFieldKlass {
  nonisolated(unsafe) let nonIsolatedUnsafeLetObject = NonSendableKlass()
  nonisolated(unsafe) var nonIsolatedUnsafeVarObject = NonSendableKlass()
  let letObject = NonSendableKlass()
  var varObject = NonSendableKlass()

  // This is unsafe since self is not main actor isolated, so our values are
  // task isolated.
  func test() async {
    await transferToMainDirect(nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(nonIsolatedUnsafeVarObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(letObject)
    // expected-tns-warning @-1 {{sending 'self.letObject' risks causing data races}}
    // expected-tns-note @-2 {{sending task-isolated 'self.letObject' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainDirect(varObject)
    // expected-tns-warning @-1 {{sending 'self.varObject' risks causing data races}}
    // expected-tns-note @-2 {{sending task-isolated 'self.varObject' to main actor-isolated global function 'transferToMainDirect' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  }

  // This is safe since self will become main actor isolated as a result of
  // test2 running.
  @MainActor func test2() async {
    await transferToMainDirect(nonIsolatedUnsafeLetObject)
    await transferToMainDirect(nonIsolatedUnsafeVarObject)
    await transferToMainDirect(letObject)
    await transferToMainDirect(varObject)
  }
}

class NonIsolatedUnsafeFieldGenericKlass<T> { // expected-complete-note 4{{}}
  nonisolated(unsafe) let nonIsolatedUnsafeLetAddressOnly: T? = nil
  nonisolated(unsafe) var nonIsolatedUnsafeVarAddressOnly: T? = nil
  let letAddressOnly: T? = nil
  var varAddressOnly: T? = nil

  // This is unsafe since self is not main actor isolated, so our values are
  // task isolated.
  func test() async {
    await transferToMainIndirect(nonIsolatedUnsafeLetAddressOnly)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'T?' into main actor-isolated context may introduce data races}}

    await transferToMainIndirect(nonIsolatedUnsafeVarAddressOnly)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'T?' into main actor-isolated context may introduce data races}}

    await transferToMainIndirect(letAddressOnly)
    // expected-tns-warning @-1 {{sending 'self.letAddressOnly' risks causing data races}}
    // expected-tns-note @-2 {{sending task-isolated 'self.letAddressOnly' to main actor-isolated global function 'transferToMainIndirect' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-complete-warning @-3 {{passing argument of non-Sendable type 'T?' into main actor-isolated context may introduce data races}}

    // TODO: This diagnostic is unfortunate since we are erroring on the
    // temporary created by the class_method call.
    await transferToMainIndirect(varAddressOnly)
    // expected-tns-warning @-1 {{sending value of non-Sendable type 'T?' risks causing data races}}
    // expected-tns-note @-2 {{sending task-isolated value of non-Sendable type 'T?' to main actor-isolated global function 'transferToMainIndirect' risks causing races in between task-isolated and main actor-isolated uses}}
  }

  // This is safe since self will become main actor isolated as a result of
  // test2 running.
  @MainActor func test2() async {
    await transferToMainIndirect(nonIsolatedUnsafeLetAddressOnly)
    await transferToMainIndirect(nonIsolatedUnsafeVarAddressOnly)
    await transferToMainIndirect(letAddressOnly)
    await transferToMainIndirect(varAddressOnly)
  }
}

////////////////////////////////
// MARK: Multiple Level Tests //
////////////////////////////////

actor ActorContainingSendableStruct {
  let x: NonIsolatedUnsafeFieldStruct? = nil

  func test() async {
    await transferToMainDirect(x!.nonIsolatedUnsafeLetObject)
    // expected-complete-warning @-1 {{passing argument of non-Sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
    await transferToMainIndirect(x)
  }
}


////////////////////
// MARK: Closures //
////////////////////

func closureTests() async {
  func sendingClosure(_ x: sending () -> ()) {
  }

  func testLetOneNSVariableError() async {
    let x = NonSendableKlass()
    sendingClosure { _ = x } // expected-warning {{sending value of non-Sendable type '() -> ()' risks causing data races}}
    // expected-note @-1 {{Passing value of non-Sendable type '() -> ()' as a 'sending' argument to local function 'sendingClosure' risks causing races in between local and caller code}}
    sendingClosure { _ = x } // expected-note {{access can happen concurrently}}
  }

  func testLetNonIsolatedUnsafeNSVariableNoError() async {
    nonisolated(unsafe) let x = NonSendableKlass()
    sendingClosure { _ = x }
    sendingClosure { _ = x }
  }

  func testLetOneNSVariableSVariableError() async {
    let x = NonSendableKlass()
    let y = CustomActorInstance()
    sendingClosure { // expected-warning {{sending value of non-Sendable type '() -> ()' risks causing data races}}
      // expected-note @-1 {{Passing value of non-Sendable type '() -> ()' as a 'sending' argument to local function 'sendingClosure' risks causing races in between local and caller code}}
      _ = x
      _ = y
    }
    sendingClosure { // expected-note {{access can happen concurrently}}
      _ = x
      _ = y
    }
  }

  func testLetNonIsolatedUnsafeNSSVariableNoError() async {
    nonisolated(unsafe) let x = NonSendableKlass()
    let y = CustomActorInstance()
    sendingClosure {
      _ = x
      _ = y
    }
    sendingClosure {
      _ = x
      _ = y
    }
  }

  func testLetTwoNSVariableError() async {
    let x = NonSendableKlass()
    let y = NonSendableKlass()
    sendingClosure { // expected-warning {{sending value of non-Sendable type '() -> ()' risks causing data races}}
      // expected-note @-1 {{Passing value of non-Sendable type '() -> ()' as a 'sending' argument to local function 'sendingClosure' risks causing races in between local and caller code}}
      _ = x
      _ = y
    }
    sendingClosure { // expected-note {{access can happen concurrently}}
      _ = x
      _ = y
    }
  }

  func testLetTwoNSVariableError2() async {
    nonisolated(unsafe) let x = NonSendableKlass()
    let y = NonSendableKlass()
    sendingClosure { // expected-warning {{sending value of non-Sendable type '() -> ()' risks causing data races}}
      // expected-note @-1 {{Passing value of non-Sendable type '() -> ()' as a 'sending' argument to local function 'sendingClosure' risks causing races in between local and caller code}}
      _ = x
      _ = y
    }
    sendingClosure { // expected-note {{access can happen concurrently}}
      _ = x
      _ = y
    }
  }

  func testLetTwoNSVariableError3() async {
    nonisolated(unsafe) let x = NonSendableKlass()
    nonisolated(unsafe) let y = NonSendableKlass()
    sendingClosure {
      _ = x
      _ = y
    }
    sendingClosure {
      _ = x
      _ = y
    }
  }

  func testVarOneNSVariableError() async {
    var x = NonSendableKlass()
    x = NonSendableKlass()

    sendingClosure { _ = x } // expected-warning {{sending value of non-Sendable type '() -> ()' risks causing data races}}
    // expected-note @-1 {{Passing value of non-Sendable type '() -> ()' as a 'sending' argument to local function 'sendingClosure' risks causing races in between local and caller code}}
    sendingClosure { _ = x } // expected-note {{access can happen concurrently}}
  }

  func testVarNonIsolatedUnsafeNSVariableNoError() async {
    nonisolated(unsafe) var x = NonSendableKlass()
    x = NonSendableKlass()

    sendingClosure { _ = x }
    sendingClosure { _ = x }
  }

  func testVarOneNSVariableSVariableError() async {
    var x = NonSendableKlass()
    x = NonSendableKlass()
    var y = CustomActorInstance()
    y = CustomActorInstance()
    sendingClosure { // expected-warning {{sending value of non-Sendable type '() -> ()' risks causing data races}}
      // expected-note @-1 {{Passing value of non-Sendable type '() -> ()' as a 'sending' argument to local function 'sendingClosure' risks causing races in between local and caller code}}
      _ = x
      _ = y
    }
    sendingClosure { // expected-note {{access can happen concurrently}}
      _ = x
      _ = y
    }
  }

  func testVarNonIsolatedUnsafeNSSVariableNoError() async {
    nonisolated(unsafe) var x = NonSendableKlass()
    x = NonSendableKlass()
    var y = CustomActorInstance()
    y = CustomActorInstance()
    sendingClosure {
      _ = x
      _ = y
    }
    sendingClosure {
      _ = x
      _ = y
    }
  }

  func testVarTwoNSVariableError() async {
    var x = NonSendableKlass()
    x = NonSendableKlass()
    var y = NonSendableKlass()
    y = NonSendableKlass()
    sendingClosure { // expected-warning {{sending value of non-Sendable type '() -> ()' risks causing data races}}
      // expected-note @-1 {{Passing value of non-Sendable type '() -> ()' as a 'sending' argument to local function 'sendingClosure' risks causing races in between local and caller code}}
      _ = x
      _ = y
    }
    sendingClosure { // expected-note {{access can happen concurrently}}
      _ = x
      _ = y
    }
  }

  func testVarTwoNSVariableError2() async {
    nonisolated(unsafe) var x = NonSendableKlass()
    x = NonSendableKlass()
    var y = NonSendableKlass()
    y = NonSendableKlass()
    sendingClosure { // expected-warning {{sending value of non-Sendable type '() -> ()' risks causing data races}}
      // expected-note @-1 {{Passing value of non-Sendable type '() -> ()' as a 'sending' argument to local function 'sendingClosure' risks causing races in between local and caller code}}
      _ = x
      _ = y
    }
    sendingClosure { // expected-note {{access can happen concurrently}}
      _ = x
      _ = y
    }
  }

  func testVarTwoNSVariableError3() async {
    nonisolated(unsafe) var x = NonSendableKlass()
    x = NonSendableKlass()
    nonisolated(unsafe) var y = NonSendableKlass()
    y = NonSendableKlass()
    sendingClosure {
      _ = x
      _ = y
    }
    sendingClosure {
      _ = x
      _ = y
    }
  }

  func testWithTaskDetached() async {
    let x1 = NonSendableKlass()
    Task.detached { _ = x1 } // expected-warning {{sending value of non-Sendable type '() async -> ()' risks causing data races}}
    // expected-note @-1 {{Passing value of non-Sendable type '() async -> ()' as a 'sending' argument to static method 'detached(priority:operation:)' risks causing races in between local and caller code}}
    Task.detached { _ = x1 } // expected-note {{access can happen concurrently}}

    nonisolated(unsafe) let x2 = NonSendableKlass()
    Task.detached { _ = x2 }
    Task.detached { _ = x2 }

    nonisolated(unsafe) let x3a = NonSendableKlass()
    nonisolated(unsafe) let x3b = NonSendableKlass()
    Task.detached { _ = x3a; _ = x3b }
    Task.detached { _ = x3a; _ = x3b }

    nonisolated(unsafe) let x4a = NonSendableKlass()
    let x4b = NonSendableKlass()
    Task.detached { _ = x4a; _ = x4b } // expected-warning {{sending value of non-Sendable type '() async -> ()' risks causing data races}}
    // expected-note @-1 {{Passing value of non-Sendable type '() async -> ()' as a 'sending' argument to static method 'detached(priority:operation:)' risks causing races in between local and caller code}}
    Task.detached { _ = x4a; _ = x4b } // expected-note {{access can happen concurrently}}
  }

  // The reason why this works is that we do not infer nonisolated(unsafe)
  // passed the begin_borrow [var_decl] of y. So we think the closure is
  // nonisolated(unsafe), but its uses via the begin_borrow [var_decl] is
  // not.
  func testNamedClosure() async {
    nonisolated(unsafe) let x = NonSendableKlass()
    let y = {
      _ = x
    }
    sendingClosure(y) // expected-warning {{sending 'y' risks causing data races}}
    // expected-note @-1 {{'y' used after being passed as a 'sending' parameter}}
    sendingClosure(y) // expected-note {{access can happen concurrently}}
  }
}
