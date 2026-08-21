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
        sendAgain(ns1)
        // expected-error@-1 {{sending 'ns1' risks causing data races}}
        // expected-note@-2 {{'ns1' is captured by a nonisolated closure. nonisolated uses in closure may race against code in the current isolation context}}
      }
    }
  }

  let _: @called(once) () -> Void = { [sending ns1] in
    manyTimes {
      calledOnce { [sending ns1] in
        sendAgain(ns1)
        // expected-error@-1 {{sending 'ns1' risks causing data races}}
        // expected-note@-2 {{'ns1' is captured by a nonisolated closure. nonisolated uses in closure may race against code in the current isolation context}}
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
      sendAgain(ns1)
      // expected-error@-1 {{sending 'ns1' risks causing data races}}
      // expected-note@-2 {{'ns1' is captured by a nonisolated closure. nonisolated uses in closure may race against code in the current isolation context}}
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

// Make sure that each PartitionOpError kind is exercised through a
// `@called(once)` closure with a `sending` capture. The goal here is not to
// pin exact wording (that's covered above) but to make sure each emitter
// produces *some* real diagnostic for this SIL shape rather than falling
// through to `emitUnknownPatternError`/`RegionIsolationUnknownPattern`.

actor CalledOnceActor {
  var ns = NS()
  func makeNS() -> NS { NS() }
}

// LocalUseAfterSend: already covered above by
// testDoubleSend/testConsumingWithUseAfterIndirectSend. No separate case
// needed here.

// SentNeverSendable: sending an actor-isolated capture (`ns` here resolves
// to `self.ns`, captured while already inside the actor-isolated method).
extension CalledOnceActor {
  func testSentNeverSendableActorIsolatedCapture() {
    _ = CalledOnceTask { [sending ns] in
      sendAgain(ns) // expected-error {{sending 'self.ns' risks causing data races}} expected-note {{'self'-isolated 'self.ns' is captured by a nonisolated closure. nonisolated uses in closure may race against later actor-isolated uses}}
    }
  }
}

// AssignNeverSendableIntoSendingResult: a `@called(once)` closure with a
// `sending` result, returning a captured value that was never itself sent.
func calledOnceResult(_ f: @called(once) () -> sending NS) {}

func testAssignNeverSendableIntoSendingResult(ns: NS) {
  calledOnceResult { ns } // expected-error {{sending 'ns' risks causing data races}} expected-note {{'ns' cannot be a 'sending' result. Code in the current task may race with caller uses}}
}

// NonSendableIsolationCrossingResult: inside a `@called(once)` closure with
// a `sending` capture, an isolation-crossing call (actor -> @concurrent)
// returns a non-Sendable result.
func testNonSendableIsolationCrossingResult(a: CalledOnceActor) async {
  _ = CalledOnceTask { [sending a] in
    Task {
      let ns = await a.makeNS() // expected-error {{non-Sendable 'NS'-typed result can not be returned from actor-isolated instance method 'makeNS()' to @concurrent context}}
      print(ns)
    }
  }
}

// InOutSendingParametersInSameRegion: two `inout sending` parameters of
// the enclosing function end up in the same region.
func testInOutSendingParametersInSameRegionUnaffected(_ x: inout sending NS, _ y: inout sending NS) {
  let _: @called(once) () -> Void = {
    x = y
  }
} // expected-error {{'inout sending' parameters 'x' and 'y' can be potentially accessed from each other at function return risking data races in caller}}
// expected-note@-1 {{caller function assumes on return that 'x' and 'y' cannot be used to access each other implying sending them to different isolation domains does not risk a data race}}

// InOutSendingNotInitializedAtExit: `inout` parameters can be captured by
// closures, including non-escaping ones, so a `sending` capture of an
// `inout sending` parameter is reachable -- sending it away via the
// `@called(once)` closure counts as consuming the parameter, and the
// function must reinitialize it with a disconnected value before returning,
// exactly as it would for an ordinary direct send.
func testInOutSendingCaptureNotReinitialized(_ x: inout sending NS) {
  _ = CalledOnceTask { [sending x] in
    // expected-error@-1 {{sending 'x' risks causing data races}}
    // expected-note@-2 {{'x' used after being passed as a 'sending' parameter; Later uses could race}}
    sendAgain(x)
  }
} // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

// ... and is accepted once `x` is reinitialized with a disconnected value
// before the function returns.
func testInOutSendingCaptureReinitialized(_ x: inout sending NS) {
  _ = CalledOnceTask { [sending x] in
    sendAgain(x)
  }
  x = NS()
}

// InOutSendingNotDisconnectedAtExit: `x` is reinitialized before return,
// but with a value (`y`) that isn't itself disconnected -- distinct from
// the "not reinitialized at all" case above.
func testInOutSendingCaptureReinitWithNonDisconnectedValue(_ x: inout sending NS, y: NS) {
  _ = CalledOnceTask { [sending x] in
    sendAgain(x)
  }
  x = y
} // expected-error {{'inout sending' parameter 'x' is accessible to code in the current isolation context at end of function}}
// expected-note@-1 {{'x' risks causing races in between code in the current isolation context and caller uses since caller assumes value is not actor isolated}}

// InOutSendingReturned: `x` is properly reinitialized, but the function
// also returns a value that aliases the region of the reinitialized `x` --
// distinct from both cases above, and from the underlying send via
// `[sending x]` itself, which is otherwise fine here.
func testInOutSendingCaptureAliasedWithReturnValue(_ x: inout sending NS) -> sending NS {
  _ = CalledOnceTask { [sending x] in
    sendAgain(x)
  }
  let fresh = NS()
  x = fresh
  return fresh // expected-error {{'fresh' cannot be returned}}
  // expected-note@-1 {{returning 'fresh' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

// IncompatibleRegionMerge: two values from different global actors, merged
// directly (not sent) inside a `@called(once)` closure body. This needs the
// closure to be neither actor-isolated (which would route through
// translateIsolatedPartialApply, sending every capture individually) nor
// have any of its captures marked `sending` (which routes the merge through
// a send diagnostic instead, via `SentNeverSendable`) -- an `@concurrent`
// `@called(once)` closure with plain, unmarked captures is what actually
// reaches `translateSILCalledOncePartialApply`'s `operandsToMerge` path and
// the underlying `Merge` PartitionOp. This diagnostic warns until a future
// language mode, not v6 (see also
// transfernonsendable_isolationhistory_incompatible_merge.swift), so it is
// a warning even here.
actor CustomActorInstance {}
@globalActor struct CustomActor { static let shared = CustomActorInstance() }

struct CalledOnceAsyncTask { init(_ ns: @called(once) () async -> Void) {} }

@MainActor
struct MergeAcrossGlobalActors {
  var mainField: NS? = nil
  @CustomActor var customField: NS? = nil

  init() {
    let a = mainField! // expected-note {{'a' is exposed to main actor-isolated code}}
    let b = customField! // expected-note {{'b' is exposed to global actor 'CustomActor'-isolated code}}
    _ = CalledOnceAsyncTask { @concurrent in
      // expected-warning@-1 {{executing operation could allow for references between values exposed to global actor 'CustomActor'-isolated code and main actor-isolated code risking data races; this will be an error in a future Swift language mode}}
      _ = a
      _ = b
    }
  }
}
