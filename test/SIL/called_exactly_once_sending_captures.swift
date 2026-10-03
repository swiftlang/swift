// RUN: %target-swift-frontend %s \
// RUN: -emit-sil -target %target-swift-5.1-abi-triple \
// RUN: -enable-experimental-feature CalledAttribute \
// RUN: -swift-version 6 \
// RUN: -verify

// REQUIRES: swift_feature_CalledAttribute
// REQUIRES: concurrency

class NS {} // expected-note {{class 'NS' does not conform to the 'Sendable' protocol}}

struct Box: ~Copyable {
  let ns: NS

  init(_ ns: NS) {
    self.ns = ns
  }

  consuming func takeNS() -> NS { ns }
}

func sendAgain(_ ns: sending NS) {}

func calledExactlyOnce(_ f: @called(exactlyOnce) () -> Void) { f() }
func manyTimes(_: () -> Void) {}

struct CalledExactlyOnceTask {
  init(_ f: @called(exactlyOnce) () -> Void) { f() }
}

func testBasic(_ ns: sending NS) {
  _ = CalledExactlyOnceTask { [sending ns] in
    sendAgain(ns)
  }
}

func testDoubleSend(_ ns: sending NS) {
  _ = CalledExactlyOnceTask { @called(exactlyOnce) [sending ns] in
    sendAgain(ns)
    // expected-error@-1 {{sending 'ns' risks causing data races}}
    // expected-note@-2 {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
    sendAgain(ns) // expected-note {{access can happen concurrently}}
  }
}

func testMultipleSendingCaptures(_ ns1: sending NS, _ ns2: sending NS) {
  _ = CalledExactlyOnceTask { @called(exactlyOnce) [sending ns1, sending ns2] in
    sendAgain(ns1)
    sendAgain(ns2)
  }
}

func testMixedSendingAndOrdinary(_ ns1: sending NS, x: Int) {
  _ = CalledExactlyOnceTask { @called(exactlyOnce) [sending ns1] in
    print(x)
    sendAgain(ns1)
  }
}

func testNestedRejectedThroughNonCalledExactlyOnceIntermediate(ns1: sending NS) {
  let outer: @called(exactlyOnce) () -> Void = {
    manyTimes {
      calledExactlyOnce { [sending ns1] in
        sendAgain(ns1)
        // expected-error@-1 {{sending 'ns1' risks causing data races}}
        // expected-note@-2 {{'ns1' is captured by a nonisolated closure. nonisolated uses in closure may race against code in the current isolation context}}
      }
    }
  }
  outer()
}

func testNestedAcceptedWithExplicitChain(ns1: sending NS) {
  let outer: @called(exactlyOnce) () -> Void = { [sending ns1] in
    calledExactlyOnce { [sending ns1] in
      sendAgain(ns1)
    }
  }
  outer()
}

func testOuterCalledExactlyOnceAloneIsNotEnough(ns1: sending NS) {
  let outer: @called(exactlyOnce) () -> Void = {
    calledExactlyOnce { [sending ns1] in
      sendAgain(ns1)
      // expected-error@-1 {{sending 'ns1' risks causing data races}}
      // expected-note@-2 {{'ns1' is captured by a nonisolated closure. nonisolated uses in closure may race against code in the current isolation context}}
    }
  }
  outer()
}

func testConsumingWithUseAfterIndirectSend(_ ns: sending NS) {
  let box = Box(ns)

  _ = CalledExactlyOnceTask { [sending box] in // expected-error {{sending 'box' risks causing data races}} expected-note {{'box' used after being passed as a 'sending' parameter; Later uses could race}}
      let ns = box.takeNS()
      sendAgain(ns)
  }

  print(ns) // expected-note {{access can happen concurrently}}
}

actor CalledExactlyOnceActor {
  var ns = NS()
  func makeNS() -> NS { NS() }
}

extension CalledExactlyOnceActor {
  func testSentNeverSendableActorIsolatedCapture() {
    _ = CalledExactlyOnceTask { [sending ns] in
      sendAgain(ns) // expected-error {{sending 'self.ns' risks causing data races}} expected-note {{'self'-isolated 'self.ns' is captured by a nonisolated closure. nonisolated uses in closure may race against later actor-isolated uses}}
    }
  }
}

func calledExactlyOnceResult(_ f: @escaping @called(exactlyOnce) () -> sending NS) {
  _ = f()
}

func testAssignNeverSendableIntoSendingResult(ns: NS) {
  calledExactlyOnceResult { ns } // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' cannot be a 'sending' result. Code in the current task may race with caller uses}}
}

func testNonSendableIsolationCrossingResult(a: CalledExactlyOnceActor) async {
  _ = CalledExactlyOnceTask { [sending a] in
    Task {
      let ns = await a.makeNS() // expected-error {{non-Sendable 'NS'-typed result can not be returned from actor-isolated instance method 'makeNS()' to @concurrent context}}
      print(ns)
    }
  }
}

func testInOutSendingCaptureNotReinitialized(_ x: inout sending NS) {
  _ = CalledExactlyOnceTask { [sending x] in
    // expected-error@-1 {{sending 'x' risks causing data races}}
    // expected-note@-2 {{'x' used after being passed as a 'sending' parameter; Later uses could race}}
    sendAgain(x)
  }
} // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

func testInOutSendingCaptureReinitialized(_ x: inout sending NS) {
  _ = CalledExactlyOnceTask { [sending x] in
    sendAgain(x)
  }
  x = NS()
}
