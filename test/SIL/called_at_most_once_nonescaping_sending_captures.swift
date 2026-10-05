// RUN: %target-swift-frontend %s \
// RUN:   -target %target-swift-5.1-abi-triple \
// RUN:   -emit-sil \
// RUN:   -enable-experimental-feature CalledAttribute \
// RUN:   -swift-version 6 \
// RUN:   -verify

// REQUIRES: swift_feature_CalledAttribute
// REQUIRES: concurrency

class NS {}

struct NCS: ~Copyable, ~Sendable {
  func test() {}
}

func useValue(_ ns: NS) {}

func useGeneric<T>(_ t: T) {}

func calledAtMostOnce(_: @called(atMostOnce) () -> Void) {}

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

func calledAtMostOnceAsync(_: @called(atMostOnce) () async -> Void) async {}

func testSentInsideBodyRemainsSent() async {
  let ns = NS()
  await calledAtMostOnceAsync { [ns] in // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
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

  await calledAtMostOnceAsync { [ns1, ns2] in // expected-error {{sending 'ns1' risks causing data races}} expected-note {{'ns1' used after being passed as a 'sending' parameter; Later uses could race}}
    await take(ns1)
    useValue(ns2)
  }

  useValue(ns1) // expected-note {{access can happen concurrently}}
  useValue(ns2) // Ok

  func merge<T>(_: T, _: T) {}
  func send(_: sending NS) {}

  let ns3 = NS()
  let ns4 = NS()

  calledAtMostOnce {
    merge(ns3, ns4)
  }

  // FIXME: The following code shouldn't be valid. `ns3` and `ns4` are merged together in the closure and shouldn't be allowed to be sent separately later.
  send(ns3)
  send(ns4)
}

actor A {
  func run(_: @called(atMostOnce) () -> Void) {}
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

  func callAtMostOnce(_ f: @called(atMostOnce) (NS) -> Void) {
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

func testGenericParameterCapture<T>(_ value: T) {
  calledAtMostOnce {
    useGeneric(value) // expected-error {{sending 'value' risks causing data races}} expected-note {{'value' is captured by a nonisolated closure. nonisolated uses in closure may race against code in the current isolation context}}
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
    v.test() // expected-error {{sending 'v' risks causing data races}}
    // expected-note@-1 {{'v' is captured by a nonisolated closure. nonisolated uses in closure may race against code in the current isolation context}}
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
