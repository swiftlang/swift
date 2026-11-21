// RUN: %target-swift-frontend %s \
// RUN:   -target %target-swift-5.1-abi-triple \
// RUN:   -emit-sil \
// RUN:   -enable-experimental-feature OnceFunctions \
// RUN:   -swift-version 6 \
// RUN:   -verify

// With opaque values, a closure can be address-lowered before its caller.
// RUN: %target-swift-frontend %s \
// RUN:   -target %target-swift-5.1-abi-triple \
// RUN:   -emit-sil \
// RUN:   -enable-experimental-feature OnceFunctions \
// RUN:   -enable-sil-opaque-values \
// RUN:   -swift-version 6 \
// RUN:   -verify

// REQUIRES: swift_feature_OnceFunctions
// REQUIRES: concurrency

class NS {}

struct NCS: ~Copyable, ~Sendable {
  func test() {}
}

func useValue(_ ns: NS) {}

func sendValue(_ ns: sending NS) {}

func useGeneric<T>(_ t: T) {}

func calledAtMostOnce(_: @callable(once) () -> Void) {}

func testNeverSentUsableAfter() {
  let ns1 = NS()

  calledAtMostOnce { [ns1] in
    useValue(ns1)
  }

  useValue(ns1) // Ok

  let ns2 = NS()
  calledAtMostOnce {
    useValue(ns2)
  }

  useValue(ns2) // Ok
}

func testPassingToTask() {
  let ns = NS()

  calledAtMostOnce { // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
    Task { _ = ns }
  }

  useValue(ns) // expected-note {{access can happen concurrently}}
}

@MainActor func take(_ ns: NS) {}

func calledAtMostOnceAsync(_: @callable(once) () async -> Void) async {}

func testSentInsideBodyRemainsSent() async {
  let ns = NS()
  await calledAtMostOnceAsync { // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
    await take(ns) // crosses an isolation boundary
  }
  useValue(ns) // expected-note {{access can happen concurrently}}
}

// Explicitly `sending` captures are always sent and never undone.
func testSendingCaptureAlwaysPermanent(_ ns: sending NS) {
  calledAtMostOnce { [sending ns] in // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
    useValue(ns)
  }
  useValue(ns) // expected-note {{access can happen concurrently}}
}

func testIndependentCapturesDoNotEntangle() async {
  let ns1 = NS()
  let ns2 = NS()

  await calledAtMostOnceAsync { // expected-error {{sending 'ns1' risks causing data races}} expected-note {{'ns1' used after being passed as a 'sending' parameter; Later uses could race}}
    await take(ns1)
    useValue(ns2)
  }

  useValue(ns1) // expected-note {{access can happen concurrently}}
  useValue(ns2) // Ok
}

func testCapturesMergedInBodyAreEntangled() {
  func merge<T>(_: T, _: T) {}
  func send(_: sending NS) {}

  let ns3 = NS()
  let ns4 = NS()

  calledAtMostOnce {
    merge(ns3, ns4)
  }

  // `ns3` and `ns4` are merged together in the closure, so they can't be sent
  // separately later.
  send(ns3) // expected-error {{sending 'ns3' risks causing data races}} expected-note {{'ns3' used after being passed as a 'sending' parameter; Later uses could race}}
  send(ns4) // expected-note {{access can happen concurrently}}
}

actor A {
  func run(_: @callable(once) () -> Void) {}
}

func testIsolationCrossingCallAlwaysSends(_ a: A) async {
  let ns = NS()
  await a.run { // expected-error {{sending value of non-Sendable type '@callable(once) () -> ()' risks causing data races}} expected-note {{sending value of non-Sendable type '@callable(once) () -> ()' to actor-isolated instance method 'run' risks causing data races between actor-isolated and local nonisolated uses}}
    useValue(ns) // never sent
  }

  // but `a.run(...)` crosses isolation which also sends `ns`
  useValue(ns) // expected-note {{access can happen concurrently}}
}

// A capture that's listed without `sending` isn't inferred to be `sending`,
// just like in a regular closure.
func testExplicitCaptureIsNotSent() {
  let ns = NS()
  calledAtMostOnce { [ns] in
    sendValue(ns) // expected-error {{sending 'ns' risks causing data races}}
    // expected-note@-1 {{'ns' is passed as a 'sending' parameter; Uses in callee may race with code in the current isolation context}}
  }

  calledAtMostOnce { [x = NS()] in
    sendValue(x) // expected-error {{sending 'x' risks causing data races}}
    // expected-note@-1 {{'x' is passed as a 'sending' parameter; Uses in callee may race with code in the current isolation context}}
  }
}

func testExplicitAndImplicitCaptures() {
  let ns1 = NS()
  let ns2 = NS()
  calledAtMostOnce { [ns1] in // expected-error {{sending 'ns2' risks causing data races}}
    // expected-note@-1 {{'ns2' used after being passed as a 'sending' parameter; Later uses could race}}
    sendValue(ns2)
    useValue(ns1)
  }

  useValue(ns1) // Ok
  useValue(ns2) // expected-note {{access can happen concurrently}}
}

func testExplicitCaptureOfParameter(_ ns: NS) {
  calledAtMostOnce { [ns] in
    useValue(ns)
  }

  useValue(ns) // Ok
}

// An entry in an outer closure's capture list isn't inferred to be `sending`
// by an inner closure that captures it implicitly either.
func testOuterClosureCaptureListEntry() {
  let ns = NS()
  calledAtMostOnce { [ns] in
    calledAtMostOnce {
      useValue(ns)
    }
    useValue(ns)
  }

  calledAtMostOnce { [ns] in
    calledAtMostOnce {
      sendValue(ns) // expected-error {{sending 'ns' risks causing data races}}
      // expected-note@-1 {{'ns' is passed as a 'sending' parameter; Uses in callee may race with code in the current isolation context}}
    }
  }
}

func testExplicitCaptureIsolationCrossingCall(_ a: A) async {
  let ns = NS()
  await a.run { [ns] in // expected-error {{sending value of non-Sendable type}}
    // expected-note@-1 {{to actor-isolated instance method 'run' risks causing data races between actor-isolated and local nonisolated uses}}
    useValue(ns)
  }

  // The send of `ns` with the closure isn't undone after the call.
  useValue(ns) // expected-note {{access can happen concurrently}}
}

func testReabstractedEscapingClosure() {
  func identity<T>(_ f: @escaping (T) -> Void) -> (T) -> Void { f }

  func callAtMostOnce(_ f: @callable(once) (NS) -> Void) {
    f(NS())
  }

  let ns = NS()

  let closure: (NS) -> Void = { x in
    useValue(ns)
    useValue(x)
  }

  callAtMostOnce(identity(closure))
  useValue(ns) // Ok (nothing is sent in the closure)
}

func checkAtMostOnce(_ condition: @autoclosure @callable(once) () -> Bool) {}

func testParameterCapturedByAutoclosure(_ ns: NS) {
  checkAtMostOnce(ns === ns) // Ok (nothing is sent in the autoclosure)
}

func sendAndCheck(_ ns: sending NS) -> Bool { true }

func testLocalSentByAutoclosure() {
  let ns = NS()
  // FIXME: The captures of an autoclosure aren't inferred to be `sending`, so
  // it can't send them, just like a regular autoclosure (rdar://193244820).
  checkAtMostOnce(sendAndCheck(ns)) // expected-error {{sending 'ns' risks causing data races}}
  // expected-note@-1 {{'ns' is passed as a 'sending' parameter; Uses in callee may race with code in the current isolation context}}
}

extension NS {
  func testSelfCapture() {
    calledAtMostOnce { useValue(self) } // Ok (nothing is sent in the closure)
    useValue(self)
  }
}

func testGenericParameterCapture<T>(_ value: T) {
  calledAtMostOnce {
    useGeneric(value) // Ok (nothing is sent in the closure)
  }

  useGeneric(value)
}

func testGenericSendingParameterCapture<T>(_ value: sending T) {
  calledAtMostOnce {
    useGeneric(value)
  }

  useGeneric(value) // Ok
}

func testVarCapturedNotMutated() {
  var value = NS()
  value = NS()
  calledAtMostOnce {
    useValue(value)
  }
  useValue(value) // Ok
}

func testNoncopyableRefAndUndo() {
  let v = NCS()
  calledAtMostOnce {
    v.test()
  }

  _ = v
}

func testNoncopyableRefAndUndoBorrowed(v: borrowing NCS) {
  calledAtMostOnce {
    v.test() // Ok (nothing is sent in the closure)
  }

  _ = v 
}

func testVarMutatedInClosure() {
  var value = NS()

  calledAtMostOnce {
    value = NS()
    useValue(value)
  }

  useValue(value) // Ok
}

// A capture that the closure hands back to its caller through an `inout
// sending` parameter or a `sending` result is sent.

func handBackInOut(_ f: @callable(once) (inout sending NS) -> Void) {
  var x = NS()
  f(&x)
  Task { [x] in useValue(x) }
}

func testCaptureHandedBackInOut() {
  let ns = NS()
  handBackInOut { $0 = ns } // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
  useValue(ns) // expected-note {{access can happen concurrently}}
}

func handBackResult<T>(_ f: @callable(once) () -> sending T) {
  let result = f()
  Task { useGeneric(result) }
}

func testCaptureHandedBackAsResult() {
  let ns = NS()
  handBackResult { ns } // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
  useValue(ns) // expected-note {{access can happen concurrently}}
}

func handBackDirectResult(_ f: @callable(once) () -> sending NS) {
  let result = f()
  Task { useValue(result) }
}

func testCaptureHandedBackAsDirectResult() {
  let ns = NS()
  handBackDirectResult { ns } // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
  useValue(ns) // expected-note {{access can happen concurrently}}
}

func testNestedClosuresDontSendCapture() {
  let ns = NS()
  calledAtMostOnce {
    calledAtMostOnce {
      useValue(ns)
    }
  }
  useValue(ns) // Ok (nothing is sent in either closure)
}

func testNestedClosureSendsCapture() {
  let ns = NS()
  calledAtMostOnce { // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
    calledAtMostOnce {
      sendValue(ns)
    }
  }
  useValue(ns) // expected-note {{access can happen concurrently}}
}

// More ways for a closure to hand a capture back to its caller.

struct HandBackError: Error {}

func handBackInOutThrowing(
  _ f: @callable(once) (inout sending NS) throws -> Void
) rethrows {
  var x = NS()
  try f(&x)
  Task { [x] in useValue(x) }
}

func testCaptureHandedBackBeforeThrowing() {
  let ns = NS()
  try? handBackInOutThrowing { $0 = ns; throw HandBackError() } // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
  useValue(ns) // expected-note {{access can happen concurrently}}
}

func testCaptureHandedBackOnOnePath(_ b: Bool) {
  let ns = NS()
  handBackInOut { if b { $0 = ns } } // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
  useValue(ns) // expected-note {{access can happen concurrently}}
}

func testCaptureHandedBackThenReplaced() {
  let ns = NS()
  handBackInOut { $0 = ns; $0 = NS() }
  useValue(ns) // Ok (the closure doesn't hand back 'ns')
}

func testCaptureHandedBackInTuple() {
  let ns = NS()
  handBackResult { (ns, 1) } // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
  useValue(ns) // expected-note {{access can happen concurrently}}
}

final class Wrapper {
  var value: NS?
}

func testCaptureHandedBackInFreshValue() {
  let ns = NS()
  handBackResult { // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
    let wrapper = Wrapper()
    wrapper.value = ns
    return wrapper
  }
  useValue(ns) // expected-note {{access can happen concurrently}}
}

func testCaptureHandedBackInArray() {
  let ns = NS()
  // FIXME: The closure hands back 'ns' in the array, so a later use of 'ns'
  // should be diagnosed. On Darwin, region analysis doesn't merge the elements
  // of an array literal into the array's region, so `func f(_ ns: NS) ->
  // sending [NS] { [ns] }` is accepted too (rdar://193244706). Other platforms
  // diagnose it, so this test doesn't use 'ns' afterwards.
  handBackResult { [ns] }
}

func handBackAsyncResult<T>(_ f: @callable(once) () async -> sending T) async {
  let result = await f()
  Task { useGeneric(result) }
}

func testCaptureHandedBackByAsyncClosure() async {
  let ns = NS()
  await handBackAsyncResult { ns } // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
  useValue(ns) // expected-note {{access can happen concurrently}}
}

// A closure can also merge a capture into an actor's region, which the caller
// can't use concurrently either.

@MainActor final class MainActorBox {
  static var value: NS?
}

func runOnMainActor(_ f: @MainActor @callable(once) () -> Void) async { await f() }

func testCaptureMergedIntoMainActorRegion() async {
  let ns = NS()
  await runOnMainActor { MainActorBox.value = ns } // expected-error {{sending 'ns' risks causing data races}}
  // expected-note@-1 {{'ns' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
  useValue(ns) // expected-note {{access can happen concurrently}}
}

@MainActor func storeOnMainActor(_ ns: NS) {
  MainActorBox.value = ns
}

func testCaptureMergedIntoMainActorRegionByCall() async {
  let ns = NS()
  await runOnMainActor { storeOnMainActor(ns) } // expected-error {{sending 'ns' risks causing data races}}
  // expected-note@-1 {{'ns' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
  useValue(ns) // expected-note {{access can happen concurrently}}
}

func testCaptureReadOnMainActor() async {
  let ns = NS()
  await runOnMainActor { print(ns) }
  useValue(ns) // Ok (the closure doesn't merge 'ns' into the main actor's region)
}

actor StateActor {
  var value: NS?
}

func withIsolatedActor(_ a: StateActor, _ f: @callable(once) (isolated StateActor) -> Void) async {
  await f(a)
}

func testCaptureMergedIntoIsolatedParameterRegion(_ a: StateActor) async {
  let ns = NS()
  await withIsolatedActor(a) { a in a.value = ns } // expected-error {{sending 'ns' risks causing data races}}
  // expected-note@-1 {{'ns' is captured by a actor-isolated closure. actor-isolated uses in closure may race against later nonisolated uses}}
  useValue(ns) // expected-note {{access can happen concurrently}}
}

// A capture that the closure hands back without `sending` stays in the
// closure's region in the caller.

func handBackNonSendingInOut(_ f: @callable(once) (inout NS) -> Void) {
  var x = NS()
  f(&x)
  // FIXME: 'x' is in the region of 'f' after the call, so sending it should be
  // diagnosed. This is also accepted for a regular closure
  // (rdar://193245037).
  Task { [x] in useValue(x) }
}

func handBackNonSendingResult(_ f: @callable(once) () -> NS) {
  let result = f()
  // FIXME: 'result' is in the region of 'f', so sending it should be
  // diagnosed. This is also accepted for a regular closure
  // (rdar://193245037).
  Task { useValue(result) }
}

func testCaptureHandedBackWithoutSending() {
  let ns = NS()
  handBackNonSendingInOut { $0 = ns }
  handBackNonSendingResult { ns }
  useValue(ns)
}

// Like the captures of any other closure, a capture that isn't sent is merged
// with the other arguments of the call that the closure is passed to.

func runWithValue(_ x: NS, _ f: @callable(once) () -> Void) {}

func testUnsentCaptureMergedWithOtherArguments() {
  func send(_: sending NS) {}

  let x = NS()
  let y = NS()
  runWithValue(x) { useValue(y) }
  send(x) // expected-error {{sending 'x' risks causing data races}} expected-note {{'x' used after being passed as a 'sending' parameter; Later uses could race}}
  useValue(y) // expected-note {{access can happen concurrently}}
}

// Likewise, the captures that an isolated closure doesn't send are merged with
// each other and with the other non-sending arguments and results of the call.

func testUnsentCapturesOfIsolatedClosureAreEntangled() async {
  func sendWrapper(_: sending Wrapper) {}

  let wrapper = Wrapper()
  let ns = NS()
  await runOnMainActor { wrapper.value = ns }
  sendValue(ns) // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
  sendWrapper(wrapper) // expected-note {{access can happen concurrently}}
}

func runOnMainActorWithValue(_ x: NS, _ f: @MainActor @callable(once) () -> Void) async {
  await f()
}

func testUnsentCaptureOfIsolatedClosureMergedWithOtherArguments() async {
  let x = NS()
  let y = NS()
  await runOnMainActorWithValue(x) { print(y) }
  sendValue(x) // expected-error {{sending 'x' risks causing data races}} expected-note {{'x' used after being passed as a 'sending' parameter; Later uses could race}}
  useValue(y) // expected-note {{access can happen concurrently}}
}

func storeResult(_ wrapper: Wrapper, _ f: @callable(once) () -> NS) {
  wrapper.value = f()
}

@MainActor func testUnsentCaptureOfIsolatedClosureMergedWithResult() {
  let wrapper = Wrapper()
  let ns = NS()
  storeResult(wrapper) { ns }
  sendValue(ns) // expected-error {{sending 'ns' risks causing data races}} expected-note {{main actor-isolated 'ns' is passed as a 'sending' parameter; Uses in callee may race with later main actor-isolated uses}}
}
