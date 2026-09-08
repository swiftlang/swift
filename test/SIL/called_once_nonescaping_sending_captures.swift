// RUN: %target-swift-frontend %s \
// RUN:   -target %target-swift-5.1-abi-triple \
// RUN:   -emit-sil \
// RUN:   -enable-experimental-feature CalledAttribute \
// RUN:   -swift-version 6 \
// RUN:   -verify

// REQUIRES: swift_feature_CalledAttribute
// REQUIRES: concurrency

class NS {}

func useValue(_ ns: NS) {}

func useGeneric<T>(_ t: T) {}

func calledOnce(_: @called(once) () -> Void) {}

func testNeverSentUsableAfter() {
  let ns1 = NS()

  calledOnce { [ns1] in
    useValue(ns1)
  }

  useValue(ns1) // Ok

  let ns2 = NS()
  calledOnce {
    useValue(ns2)
  }

  useValue(ns2) // Ok
}

func testPassingToTask() {
  let ns = NS()

  calledOnce { // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
    Task { _ = ns }
  }

  useValue(ns) // expected-note {{access can happen concurrently}}
}

@MainActor func take(_ ns: NS) {}

func calledOnceAsync(_: @called(once) () async -> Void) async {}

func testSentInsideBodyRemainsSent() async {
  let ns = NS()
  await calledOnceAsync { [ns] in // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
    await take(ns) // crosses an isolation boundary
  }
  useValue(ns) // expected-note {{access can happen concurrently}}
}

// Explicitly `sending` captures are always sent and never undone.
func testSendingCaptureAlwaysPermanent(_ ns: sending NS) {
  calledOnce { [sending ns] in // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
    useValue(ns)
  }
  useValue(ns) // expected-note {{access can happen concurrently}}
}

func testIndependentCapturesDoNotEntangle() async {
  let ns1 = NS()
  let ns2 = NS()

  await calledOnceAsync { [ns1, ns2] in // expected-error {{sending 'ns1' risks causing data races}} expected-note {{'ns1' used after being passed as a 'sending' parameter; Later uses could race}}
    await take(ns1)
    useValue(ns2)
  }

  useValue(ns1) // expected-note {{access can happen concurrently}}
  useValue(ns2) // Ok

  func merge<T>(_: T, _: T) {}
  func send(_: sending NS) {}

  let ns3 = NS()
  let ns4 = NS()

  calledOnce {
    merge(ns3, ns4)
  }

  // FIXME: The following code shouldn't be valid. `ns3` and `ns4` are merged together in the closure and shouldn't be allowed to be sent separately later.
  send(ns3)
  send(ns4)
}

actor A {
  func run(_: @called(once) () -> Void) {}
}

func testIsolationCrossingCallAlwaysSends(_ a: A) async {
  let ns = NS()
  await a.run { [ns] in // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
    useValue(ns) // never sent
  }

  // but `a.run(...)` crosses isolation which also sends `ns`
  useValue(ns) // expected-note {{access can happen concurrently}}
}

func testReabstractedEscapingClosure() {
  func identity<T>(_ f: @escaping (T) -> Void) -> (T) -> Void { f }

  func callOnce(_ f: @called(once) (NS) -> Void) {
    f(NS())
  }

  let ns = NS()

  let closure: (NS) -> Void = { x in
    useValue(ns)
    useValue(x)
  }

  callOnce(identity(closure))
  useValue(ns) // Ok (nothing is sent in the closure)
}

func testGenericParameterCapture<T>(_ value: T) {
  calledOnce {
    useGeneric(value) // expected-error {{sending 'value' risks causing data races}} expected-note {{'value' is captured by a nonisolated closure. nonisolated uses in closure may race against code in the current isolation context}}
  }

  useGeneric(value)
}

func testGenericSendingParameterCapture<T>(_ value: sending T) {
  calledOnce {
    useGeneric(value)
  }

  useGeneric(value) // Ok
}

func testVarCapturedNotMutated() {
  var value = NS()
  value = NS()
  calledOnce {
    useValue(value)
  }
  useValue(value) // Ok
}

func testNoncopyableRefAndUndo() {
  struct NCS: ~Copyable, ~Sendable {
    func test() {}
  }

  // FIXME: There should be no errors here. This is currently considered to be
  // a consuming use of `v` because `@called(once)` is never marked as `[on_stack]`.
  // The move-only checker needs to be tought about non-escaping `@called(once)`.
  let v = NCS() // expected-error {{'v' used after consume}}
  calledOnce { // expected-note {{consumed here}}
    v.test()
  }

  _ = v // expected-note {{used here}}
}

func testVarMutatedInClosure() {
  var value = NS()

  calledOnce {
    value = NS()
    useValue(value)
  }

  useValue(value) // Ok
}
