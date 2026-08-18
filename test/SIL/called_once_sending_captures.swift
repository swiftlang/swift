// RUN: %target-swift-frontend %s \
// RUN: -emit-sil -target %target-swift-5.1-abi-triple \
// RUN: -enable-experimental-feature CalledAttribute \
// RUN: -swift-version 6 \
// RUN: -verify

// REQUIRES: swift_feature_CalledAttribute
// REQUIRES: concurrency

class NS {}

struct Box: ~Copyable {
  let ns: NS

  init(_ ns: NS) {
    self.ns = ns
  }

  consuming func takeNS() -> NS { ns }
}

func sendAgain(_ ns: sending NS) {}

func calledOnce(_: @called(once) () -> Void) {}
func manyTimes(_: () -> Void) {}

struct CalledOnceTask {
  init(_ ns: @called(once) () -> Void) {}
}

func testBasic(_ ns: sending NS) {
  _ = CalledOnceTask { [sending ns] in
    sendAgain(ns)
  }
}

func testDoubleSend(_ ns: sending NS) {
  _ = CalledOnceTask { @called(once) [sending ns] in
    sendAgain(ns)
    // expected-error@-1 {{sending 'ns' risks causing data races}}
    // expected-note@-2 {{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
    sendAgain(ns) // expected-note {{access can happen concurrently}}
  }
}

func testMultipleSendingCaptures(_ ns1: sending NS, _ ns2: sending NS) {
  _ = CalledOnceTask { @called(once) [sending ns1, sending ns2] in
    sendAgain(ns1)
    sendAgain(ns2)
  }
}

func testMixedSendingAndOrdinary(_ ns1: sending NS, x: Int) {
  _ = CalledOnceTask { @called(once) [sending ns1] in
    print(x)
    sendAgain(ns1)
  }
}

func testNestedRejectedThroughNonCalledOnceIntermediate(ns1: sending NS) {
  let _: @called(once) () -> Void = {
    manyTimes {
      calledOnce { [sending ns1] in
        // expected-error@-1 {{task or actor-isolated value cannot be sent}}
        sendAgain(ns1)
      }
    }
  }
  
  let _: @called(once) () -> Void = { [sending ns1] in
    manyTimes {
      calledOnce { [sending ns1] in
        // expected-error@-1 {{task or actor-isolated value cannot be sent}}
        sendAgain(ns1)
      }
    }
  }
}

func testNestedAcceptedWithExplicitChain(ns1: sending NS) {
  let _: @called(once) () -> Void = { [sending ns1] in
    calledOnce { [sending ns1] in
      sendAgain(ns1)
    }
  }
}

func testOuterCalledOnceAloneIsNotEnough(ns1: sending NS) {
  let _: @called(once) () -> Void = {
    calledOnce { [sending ns1] in
      // expected-error@-1 {{task or actor-isolated value cannot be sent}}
      sendAgain(ns1)
    }
  }
}

func testConsumingWithUseAfterIndirectSend(_ ns: sending NS) {
  let box = Box(ns)

  _ = CalledOnceTask { [sending box] in // expected-error {{sending 'box' risks causing data races}} expected-note {{'box' used after being passed as a 'sending' parameter; Later uses could race}}
      let ns = box.takeNS()
      sendAgain(ns)
  }

  print(ns) // expected-note {{access can happen concurrently}}
}
