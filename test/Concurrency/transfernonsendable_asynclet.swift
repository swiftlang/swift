// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -verify -verify-additional-prefix complete- %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -enable-experimental-feature RegionBasedIsolation -disable-availability-checking -verify -verify-additional-prefix tns-  %s -o /dev/null

// REQUIRES: concurrency
// REQUIRES: asserts

////////////////////////
// MARK: Declarations //
////////////////////////

/// Classes are always non-sendable, so this is non-sendable
class NonSendableKlass { // expected-complete-note 92{{}}
  var field: NonSendableKlass? = nil
  var field2: NonSendableKlass? = nil

  func asyncCall() async {}
}

class SendableKlass : @unchecked Sendable {}

actor Actor {
  var klass = NonSendableKlass()
  final var finalKlass = NonSendableKlass()

  func useKlass(_ x: NonSendableKlass) {}

  func useSendableFunction(_: @Sendable () -> Void) {}
  func useNonSendableFunction(_: () -> Void) {}
}

final actor FinalActor {
  var klass = NonSendableKlass()
  func useKlass(_ x: NonSendableKlass) {}
}

func useInOut<T>(_ x: inout T) {}
@discardableResult
func useValue<T>(_ x: T) -> T { x }
func useValueWrapInOptional<T>(_ x: T) -> T? { x }

@MainActor func transferToMain<T>(_ t: T) async {}
@MainActor func transferToMainInt<T>(_ t: T) async -> Int { 5 }
@MainActor func transferToMainIntOpt<T>(_ t: T) async -> Int? { 5 }

func transferToNonIsolated<T>(_ t: T) async {}
func transferToNonIsolatedInt<T>(_ t: T) async -> Int { 5 }
func transferToNonIsolatedIntOpt<T>(_ t: T) async -> Int? { 5 }

var booleanFlag: Bool { false }

struct SingleFieldKlassBox {
  var k = NonSendableKlass()
}

struct TwoFieldKlassBox { // expected-complete-note 24{{}}
  var k1 = NonSendableKlass()
  var k2 = NonSendableKlass()
}

/////////////////////////////////////
// MARK: Async Let Let Actor Tests //
/////////////////////////////////////

func asyncLet_Let_ActorIsolated_Simple1() async {
  let x = NonSendableKlass()
  async let y = transferToMainInt(x) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  useValue(x) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_ActorIsolated_Simple2() async {
  let x = NonSendableKlass()
  async let y = transferToMainInt(x) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  let _ = await y
  useValue(x) // expected-tns-note {{access here could race}}
}

func asyncLet_Let_ActorIsolated_Simple3() async {
  let x = NonSendableKlass()
  async let y = transferToMainInt(x) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  // TODO: We shouldn't emit the 2nd error here given the current implementation
  // since it is only accessible along the else path but we already hit
  // useValue. But when we search for requires that we want to emit, we do not
  // take into account awaits. That being said, even though this is
  // inconsistent, a race does occur here.
  if await booleanFlag {
    let _ = await y
  } else {
    useValue(x) // expected-tns-note {{access here could race}}
  }
  useValue(x) // expected-tns-note {{access here could race}}
}

func asyncLet_Let_ActorIsolated_Simple4() async {
  let x = NonSendableKlass()
  async let y = transferToMainInt(x) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  if await booleanFlag {
    useValue(x) // expected-tns-note {{access here could race}}
    let _ = await y
  } else {
    useValue(x) // expected-tns-note {{access here could race}}
  }
}

func asyncLet_Let_ActorIsolated_Simple5() async {
  let x = NonSendableKlass()
  async let y = transferToMainInt(x) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  if await booleanFlag {
    let _ = await y
    useValue(x) // expected-tns-note {{access here could race}}
  } else {
    useValue(x) // expected-tns-note {{access here could race}}
  }
}

// Make sure we error appropriately when accessing a field of a class in the
// async let rather than the base class.
func asyncLet_Let_ActorIsolated_AccessFieldsClass1() async {
  let x = NonSendableKlass()
  async let y = transferToMainInt(x.field) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass?' into main actor-isolated context may introduce data races}}
  useValue(x) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_ActorIsolated_AccessFieldsClass2() async {
  let x = NonSendableKlass()
  async let y = transferToMainInt(x.field) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass?' into main actor-isolated context may introduce data races}}
  useValue(x.field) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_ActorIsolated_AccessFieldsClass3() async {
  let x = NonSendableKlass()
  async let y = transferToMainInt(x.field) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass?' into main actor-isolated context may introduce data races}}
  useValue(x.field2) // expected-tns-note {{access here could race}}
  let _ = await y
}

// Make sure we error appropriately when accessing a field of a struct in the
// async let rather than the base struct.
func asyncLet_Let_ActorIsolated_AccessFieldsStruct1() async {
  let x = TwoFieldKlassBox()
  async let y = transferToMainInt(x.k1) // expected-tns-warning {{passing argument of non-sendable type 'TwoFieldKlassBox' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'TwoFieldKlassBox' in 'async let' binding}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  useValue(x) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_ActorIsolated_AccessFieldsStruct2() async {
  let x = TwoFieldKlassBox()
  async let y = transferToMainInt(x.k1) // expected-tns-warning {{passing argument of non-sendable type 'TwoFieldKlassBox' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'TwoFieldKlassBox' in 'async let' binding}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  useValue(x.k1) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_ActorIsolated_AccessFieldsStruct3() async {
  let x = TwoFieldKlassBox()
  async let y = transferToMainInt(x.k1) // expected-tns-warning {{passing argument of non-sendable type 'TwoFieldKlassBox' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'TwoFieldKlassBox' in 'async let' binding}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  useValue(x.k2) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_ActorIsolated_AccessFieldsStruct4() async {
  let x = TwoFieldKlassBox()
  async let y = transferToMainInt(x.k2.field2) // expected-tns-warning {{passing argument of non-sendable type 'TwoFieldKlassBox' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'TwoFieldKlassBox' in 'async let' binding}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass?' into main actor-isolated context may introduce data races}}
  useValue(x.k1.field) // expected-tns-note {{access here could race}}
  let _ = await y
}

// Make sure we error appropriately when accessing a field of a struct in the
// async let rather than the base struct.
func asyncLet_Let_ActorIsolated_AccessFieldsTuple1() async {
  let x = (TwoFieldKlassBox(), TwoFieldKlassBox())
  async let y = transferToMainInt(x.0.k1) // expected-tns-warning {{passing argument of non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' in 'async let' binding}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  // expected-complete-warning @-3 {{capture of 'x' with non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' in 'async let' binding}}
  useValue(x) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_ActorIsolated_AccessFieldsTuple2() async {
  let x = (TwoFieldKlassBox(), TwoFieldKlassBox())
  async let y = transferToMainInt(x.0.k1) // expected-tns-warning {{passing argument of non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' in 'async let' binding}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  // expected-complete-warning @-3 {{capture of 'x' with non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' in 'async let' binding}}
  useValue(x.1) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_ActorIsolated_AccessFieldsTuple3() async {
  let x = (TwoFieldKlassBox(), TwoFieldKlassBox())
  async let y = transferToMainInt(x.0.k1) // expected-tns-warning {{passing argument of non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' in 'async let' binding}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  // expected-complete-warning @-3 {{capture of 'x' with non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' in 'async let' binding}}
  useValue(x.1.k2) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_ActorIsolated_AccessFieldsTuple4() async {
  let x = (TwoFieldKlassBox(), TwoFieldKlassBox())
  async let y = transferToMainInt(x.1.k1.field2) // expected-tns-warning {{passing argument of non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' in 'async let' binding}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass?' into main actor-isolated context may introduce data races}}
  // expected-complete-warning @-3 {{capture of 'x' with non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' in 'async let' binding}}
  useValue(x.0.k2.field) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_ActorIsolated_CallBuriedInOtherExpr1() async {
  let x = NonSendableKlass()
  let x2 = NonSendableKlass()

  async let y = useValue(transferToMainInt(x) + transferToMainInt(x2)) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-tns-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-2 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-3 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  // expected-complete-warning @-4 {{capture of 'x2' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-5 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  useValue(x) // expected-tns-note {{access here could race}}
  let _ = await y
  useValue(x2) // expected-tns-note {{access here could race}}
}

func asyncLet_Let_ActorIsolated_CallBuriedInOtherExpr2() async {
  let x = NonSendableKlass()
  let x2 = NonSendableKlass()

  async let y = useValue(transferToMainInt(x) + transferToMainInt(x2)) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-tns-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-2 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-3 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  // expected-complete-warning @-4 {{capture of 'x2' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-5 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  useValue(x2) // expected-tns-note {{access here could race}}
  let _ = await y
  useValue(x) // expected-tns-note {{access here could race}}
}

// Make sure we emit separate errors for x and x2.
func asyncLet_Let_ActorIsolated_CallBuriedInOtherExpr3() async {
  let x = NonSendableKlass()
  let x2 = NonSendableKlass()

  async let y = useValue(transferToMainInt(x) + transferToMainInt(x2)) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-tns-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}

  // expected-complete-warning @-3 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-4 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  // expected-complete-warning @-5 {{capture of 'x2' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-6 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  // We only error on the first value captured if multiple values are captured
  // since we track a single partial_apply as a transfer instruction.
  useValue(x) // expected-tns-note {{access here could race}}
  useValue(x2) // expected-tns-note {{access here could race}}
  let _ = await y
}

// Make sure that we emit an error for transferToMainInt in the async val
// function itself.
func asyncLet_Let_ActorIsolated_CallBuriedInOtherExpr4() async {
  let x = NonSendableKlass()

  async let y = useValue(transferToMainInt(x) + transferToMainInt(x)) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-tns-note @-1:67 {{access here could race}}

  // expected-complete-warning @-3 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-4 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  // expected-complete-warning @-5 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  let _ = await y
}

// Make sure that we emit an error when the same value is used by two async let
// as part of one statement.
func asyncLet_Let_ActorIsolated_MultipleAsyncLet1() async {
  let x = NonSendableKlass()

  async let y = useValue(transferToMainInt(x)), z = useValue(transferToMainInt(x)) // expected-tns-warning {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-tns-note @-1:53 {{access here could race}}

  // expected-complete-warning @-3 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-4 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-5 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  // expected-complete-warning @-6 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  let _ = await y
  let _ = await z
}

// Make sure we don't error when we use different values in different async let.
func asyncLet_Let_ActorIsolated_MultipleAsyncLet2() async {
  let x = NonSendableKlass()
  let x2 = NonSendableKlass()

  async let y = useValue(transferToMainInt(x)), z = useValue(transferToMainInt(x2))
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  // expected-complete-warning @-3 {{capture of 'x2' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-4 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  let _ = await y
  let _ = await z
}

func asyncLet_Let_ActorIsolated_MultipleAsyncLet3() async {
  let x = NonSendableKlass()
  let x2 = NonSendableKlass()

  async let y = useValue(transferToMainInt(x)), z = useValue(transferToMainInt(x2))
  // expected-tns-warning @-1:53 {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}

  // expected-complete-warning @-3 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-4 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  // expected-complete-warning @-5 {{capture of 'x2' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-6 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  useValue(x2) // expected-tns-note {{access here could race}}
  let _ = await y
  let _ = await z
}

func asyncLet_Let_ActorIsolated_MultipleAsyncLet4() async {
  let x = NonSendableKlass()
  let x2 = NonSendableKlass()

  async let y = useValue(transferToMainInt(x)), z = useValue(transferToMainInt(x2))
  // expected-tns-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-tns-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-3 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-4 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  // expected-complete-warning @-5 {{capture of 'x2' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-6 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  let _ = await y
  let _ = await z
  useValue(x) // expected-tns-note {{access here could race}}
  useValue(x2) // expected-tns-note {{access here could race}}
}

func asyncLet_Let_ActorIsolated_MultipleAsyncLet5() async {
  let x = NonSendableKlass()
  let x2 = NonSendableKlass()

  async let y = useValue(transferToMainInt(x)), z = useValue(transferToMainInt(x2))
  // expected-tns-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-tns-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-complete-warning @-3 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-4 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  // expected-complete-warning @-5 {{capture of 'x2' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-6 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  let _ = await y
  useValue(x) // expected-tns-note {{access here could race}}
  let _ = await z
  useValue(x2) // expected-tns-note {{access here could race}}
}

func asyncLet_Let_ActorIsolated_MultipleAsyncLet6() async {
  let x = NonSendableKlass()
  let x2 = NonSendableKlass()

  async let y = useValue(transferToMainInt(x)), z = useValue(transferToMainInt(x2))
  // expected-tns-warning @-1 {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}
  // expected-tns-warning @-2 {{passing argument of non-sendable type 'NonSendableKlass' from nonisolated context to main actor-isolated context at this call site could yield a race with accesses later in this function}}

  // expected-complete-warning @-4 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-5 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}
  // expected-complete-warning @-6 {{capture of 'x2' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-7 {{passing argument of non-sendable type 'NonSendableKlass' into main actor-isolated context may introduce data races}}

  let _ = await y
  useValue(x) // expected-tns-note {{access here could race}}
  useValue(x2) // expected-tns-note {{access here could race}}
  let _ = await z
}

///////////////////////////////////////
// MARK: SendNonSendable NonIsolated //
///////////////////////////////////////

func asyncLet_Let_NonIsolated_Simple1() async {
  let x = NonSendableKlass()
  async let y = transferToNonIsolatedInt(x) // expected-tns-warning {{transferred value of non-Sendable type 'NonSendableKlass' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}

  useValue(x) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_NonIsolated_Simple2() async {
  let x = NonSendableKlass()
  async let y = transferToNonIsolatedInt(x)
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}

  let _ = await y
  useValue(x)
}

func asyncLet_Let_NonIsolated_Simple3() async {
  let x = NonSendableKlass()
  async let y = transferToNonIsolatedInt(x) // expected-tns-warning {{transferred value of non-Sendable type 'NonSendableKlass' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}

  // TODO: We shouldn't emit the 2nd error here given the current implementation
  // since it is only accessible along the else path but we already hit
  // useValue. But when we search for requires that we want to emit, we do not
  // take into account awaits. That being said, even though this is
  // inconsistent, a race does occur here.
  if await booleanFlag {
    let _ = await y
  } else {
    useValue(x) // expected-tns-note {{access here could race}}
  }
  useValue(x) // expected-tns-note {{access here could race}}
}

func asyncLet_Let_NonIsolated_Simple4() async {
  let x = NonSendableKlass()
  async let y = transferToNonIsolatedInt(x) // expected-tns-warning {{transferred value of non-Sendable type 'NonSendableKlass' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}

  if await booleanFlag {
    useValue(x) // expected-tns-note {{access here could race}}
    let _ = await y
  } else {
    useValue(x) // expected-tns-note {{access here could race}}
  }
}

func asyncLet_Let_NonIsolated_Simple5() async {
  let x = NonSendableKlass()
  async let y = transferToNonIsolatedInt(x) // expected-tns-warning {{transferred value of non-Sendable type 'NonSendableKlass' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}

  if await booleanFlag {
    let _ = await y
    useValue(x)
  } else {
    useValue(x) // expected-tns-note {{access here could race}}
  }
}

// Make sure we error appropriately when accessing a field of a class in the
// async let rather than the base class.
func asyncLet_Let_NonIsolated_AccessFieldsClass1() async {
  let x = NonSendableKlass()
  async let y = transferToNonIsolatedInt(x.field) // expected-tns-warning {{transferred value of non-Sendable type 'NonSendableKlass' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}

  useValue(x) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_NonIsolated_AccessFieldsClass2() async {
  let x = NonSendableKlass()
  async let y = transferToNonIsolatedInt(x.field) // expected-tns-warning {{transferred value of non-Sendable type 'NonSendableKlass' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}

  useValue(x.field) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_NonIsolated_AccessFieldsClass3() async {
  let x = NonSendableKlass()
  async let y = transferToNonIsolatedInt(x.field) // expected-tns-warning {{transferred value of non-Sendable type 'NonSendableKlass' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}

  useValue(x.field2) // expected-tns-note {{access here could race}}
  let _ = await y
}

// Make sure we error appropriately when accessing a field of a struct in the
// async let rather than the base struct.
func asyncLet_Let_NonIsolated_AccessFieldsStruct1() async {
  let x = TwoFieldKlassBox()
  async let y = transferToNonIsolatedInt(x.k1) // expected-tns-warning {{transferred value of non-Sendable type 'TwoFieldKlassBox' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'TwoFieldKlassBox' in 'async let' binding}}

  useValue(x) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_NonIsolated_AccessFieldsStruct2() async {
  let x = TwoFieldKlassBox()
  async let y = transferToNonIsolatedInt(x.k1) // expected-tns-warning {{transferred value of non-Sendable type 'TwoFieldKlassBox' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'TwoFieldKlassBox' in 'async let' binding}}

  useValue(x.k1) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_NonIsolated_AccessFieldsStruct3() async {
  let x = TwoFieldKlassBox()
  async let y = transferToNonIsolatedInt(x.k1) // expected-tns-warning {{transferred value of non-Sendable type 'TwoFieldKlassBox' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'TwoFieldKlassBox' in 'async let' binding}}

  useValue(x.k2) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_NonIsolated_AccessFieldsStruct4() async {
  let x = TwoFieldKlassBox()
  async let y = transferToNonIsolatedInt(x.k2.field2) // expected-tns-warning {{transferred value of non-Sendable type 'TwoFieldKlassBox' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'TwoFieldKlassBox' in 'async let' binding}}

  useValue(x.k1.field) // expected-tns-note {{access here could race}}
  let _ = await y
}

// Make sure we error appropriately when accessing a field of a struct in the
// async let rather than the base struct.
func asyncLet_Let_NonIsolated_AccessFieldsTuple1() async {
  let x = (TwoFieldKlassBox(), TwoFieldKlassBox())
  async let y = transferToNonIsolatedInt(x.0.k1) // expected-tns-warning {{transferred value of non-Sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' in 'async let' binding}}

  // expected-complete-warning @-3 {{capture of 'x' with non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' in 'async let' binding}}
  useValue(x) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_NonIsolated_AccessFieldsTuple2() async {
  let x = (TwoFieldKlassBox(), TwoFieldKlassBox())
  async let y = transferToNonIsolatedInt(x.0.k1) // expected-tns-warning {{transferred value of non-Sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' in 'async let' binding}}
  // expected-complete-warning @-2 {{capture of 'x' with non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' in 'async let' binding}}
  useValue(x.1) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_NonIsolated_AccessFieldsTuple3() async {
  let x = (TwoFieldKlassBox(), TwoFieldKlassBox())
  async let y = transferToNonIsolatedInt(x.0.k1) // expected-tns-warning {{transferred value of non-Sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' in 'async let' binding}}
  // expected-complete-warning @-2 {{capture of 'x' with non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' in 'async let' binding}}

  useValue(x.1.k2) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_NonIsolated_AccessFieldsTuple4() async {
  let x = (TwoFieldKlassBox(), TwoFieldKlassBox())
  async let y = transferToNonIsolatedInt(x.1.k1.field2) // expected-tns-warning {{transferred value of non-Sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' in 'async let' binding}}
  // expected-complete-warning @-2 {{capture of 'x' with non-sendable type '(TwoFieldKlassBox, TwoFieldKlassBox)' in 'async let' binding}}
  useValue(x.0.k2.field) // expected-tns-note {{access here could race}}
  let _ = await y
}

func asyncLet_Let_NonIsolated_CallBuriedInOtherExpr1() async {
  let x = NonSendableKlass()
  let x2 = NonSendableKlass()

  async let y = useValue(transferToNonIsolatedInt(x) + transferToNonIsolatedInt(x2)) // expected-tns-warning {{transferred value of non-Sendable type 'NonSendableKlass' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-2 {{capture of 'x2' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  useValue(x) // expected-tns-note {{access here could race}}
  let _ = await y
  useValue(x2)
}

func asyncLet_Let_NonIsolated_CallBuriedInOtherExpr2() async {
  let x = NonSendableKlass()
  let x2 = NonSendableKlass()

  async let y = useValue(transferToNonIsolatedInt(x) + transferToNonIsolatedInt(x2)) // expected-tns-warning {{transferred value of non-Sendable type 'NonSendableKlass' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-2 {{capture of 'x2' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  useValue(x2) // expected-tns-note {{access here could race}}
  let _ = await y
  useValue(x)
}

// Make sure we emit separate errors for x and x2.
func asyncLet_Let_NonIsolated_CallBuriedInOtherExpr3() async {
  let x = NonSendableKlass()
  let x2 = NonSendableKlass()

  async let y = useValue(transferToNonIsolatedInt(x) + transferToNonIsolatedInt(x2)) // expected-tns-warning {{transferred value of non-Sendable type 'NonSendableKlass' that could race with later accesses}}
  // expected-tns-warning @-1 {{transferred value of non-Sendable type 'NonSendableKlass' that could race with later accesses}}

  // expected-complete-warning @-3 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-4 {{capture of 'x2' with non-sendable type 'NonSendableKlass' in 'async let' binding}}

  // We only error on the first value captured if multiple values are captured
  // since we track a single partial_apply as a transfer instruction.
  useValue(x) // expected-tns-note {{access here could race}}
  useValue(x2) // expected-tns-note {{access here could race}}
  let _ = await y
}

// Make sure that we emit an error for transferToNonIsolatedInt in the async val
// function itself.
func asyncLet_Let_NonIsolated_CallBuriedInOtherExpr4() async {
  let x = NonSendableKlass()

  async let y = useValue(transferToNonIsolatedInt(x) + transferToNonIsolatedInt(x))
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}

  let _ = await y
}

// Make sure that we emit an error when the same value is used by two async let
// as part of one statement.
func asyncLet_Let_NonIsolated_MultipleAsyncLet1() async {
  let x = NonSendableKlass()

  async let y = useValue(transferToNonIsolatedInt(x)), z = useValue(transferToNonIsolatedInt(x)) // expected-tns-warning {{transferred value of non-Sendable type 'NonSendableKlass' that could race with later accesses}}
  // expected-tns-note @-1:60 {{access here could race}}

  // expected-complete-warning @-3 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-4 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}

  let _ = await y
  let _ = await z
}

// Make sure we don't error when we use different values in different async let.
func asyncLet_Let_NonIsolated_MultipleAsyncLet2() async {
  let x = NonSendableKlass()
  let x2 = NonSendableKlass()

  async let y = useValue(transferToNonIsolatedInt(x)), z = useValue(transferToNonIsolatedInt(x2))
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-2 {{capture of 'x2' with non-sendable type 'NonSendableKlass' in 'async let' binding}}

  let _ = await y
  let _ = await z
}

func asyncLet_Let_NonIsolated_MultipleAsyncLet3() async {
  let x = NonSendableKlass()
  let x2 = NonSendableKlass()

  async let y = useValue(transferToNonIsolatedInt(x)), z = useValue(transferToNonIsolatedInt(x2))
  // expected-tns-warning @-1:60 {{transferred value of non-Sendable type 'NonSendableKlass' that could race with later accesses}}

  // expected-complete-warning @-3 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-4 {{capture of 'x2' with non-sendable type 'NonSendableKlass' in 'async let' binding}}

  useValue(x2) // expected-tns-note {{access here could race}}
  let _ = await y
  let _ = await z
}

func asyncLet_Let_NonIsolated_MultipleAsyncLet4() async {
  let x = NonSendableKlass()
  let x2 = NonSendableKlass()

  async let y = useValue(transferToNonIsolatedInt(x)), z = useValue(transferToNonIsolatedInt(x2))
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-2 {{capture of 'x2' with non-sendable type 'NonSendableKlass' in 'async let' binding}}

  let _ = await y
  let _ = await z
  useValue(x)
  useValue(x2)
}

func asyncLet_Let_NonIsolated_MultipleAsyncLet5() async {
  let x = NonSendableKlass()
  let x2 = NonSendableKlass()

  async let y = useValue(transferToNonIsolatedInt(x)), z = useValue(transferToNonIsolatedInt(x2))
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-2 {{capture of 'x2' with non-sendable type 'NonSendableKlass' in 'async let' binding}}

  let _ = await y
  useValue(x)
  let _ = await z
  useValue(x2)
}

func asyncLet_Let_NonIsolated_MultipleAsyncLet6() async {
  let x = NonSendableKlass()
  let x2 = NonSendableKlass()

  async let y = useValue(transferToNonIsolatedInt(x)), z = useValue(transferToNonIsolatedInt(x2))
  // expected-tns-warning @-1 {{transferred value of non-Sendable type 'NonSendableKlass' that could race with later accesses}}

  // expected-complete-warning @-3 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  // expected-complete-warning @-4 {{capture of 'x2' with non-sendable type 'NonSendableKlass' in 'async let' binding}}

  let _ = await y
  useValue(x)
  useValue(x2) // expected-tns-note {{access here could race}}
  let _ = await z
}

////////////////////////////
// MARK: Normal Value Use //
////////////////////////////

func asyncLet_Let_NormalUse_Simple1() async {
  let x = NonSendableKlass()
  async let y = x // expected-tns-warning {{transferred value of non-Sendable type 'NonSendableKlass' that could race with later accesses}}
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  useValue(x) // expected-tns-note {{access here could race}}
  let _ = await y

}

func asyncLet_Let_NormalUse_Simple2() async {
  let x = NonSendableKlass()
  async let y = x
  // expected-complete-warning @-1 {{capture of 'x' with non-sendable type 'NonSendableKlass' in 'async let' binding}}
  let _ = await y
  useValue(x)
}
