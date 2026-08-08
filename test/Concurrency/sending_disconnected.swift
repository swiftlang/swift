// RUN: %target-swift-frontend -emit-sil -swift-version 6 -target %target-swift-5.1-abi-triple -verify %s -o /dev/null -parse-as-library

// REQUIRES: concurrency
// REQUIRES: synchronization

import Synchronization

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {}

func useValue<T>(_ t: T) {}

/////////////////
// MARK: Tests //
/////////////////

// Construction from a freshly-built disconnected value is OK.
@available(SwiftStdlib 6.5, *)
@MainActor
func disconnected_construct_freshValue() {
  _ = Disconnected(NonSendableKlass())
}

// `take()`'s result is `sending`, so it can cross an isolation boundary.
@available(SwiftStdlib 6.5, *)
@MainActor
func disconnected_takeProducesSending() async {
  let d = Disconnected(NonSendableKlass())
  let v = d.take()
  await Task.detached {
    useValue(v)
  }.value
}

// `swap()`'s result is `sending`, so it can cross an isolation boundary.
@available(SwiftStdlib 6.5, *)
@MainActor
func disconnected_swapProducesSending() async {
  var d = Disconnected(NonSendableKlass())
  let old = d.swap(newValue: NonSendableKlass())
  await Task.detached {
    useValue(old)
  }.value
}

// `init` takes `sending`. Touching the argument after it has been transferred
// into the wrapper is a region-isolation violation.
@available(SwiftStdlib 6.5, *)
@MainActor
func disconnected_init_isSending() {
  let x = NonSendableKlass()
  let d = Disconnected(x)
  // expected-error @-1 {{sending 'x' risks causing data races}}
  // expected-note @-2 {{'x' used after being passed as a 'sending' parameter}}
  useValue(x) // expected-note {{access can happen concurrently}}
  _ = d
}

// `swap`'s `newValue` parameter is `sending`. Touching the argument after the
// swap is a region-isolation violation.
@available(SwiftStdlib 6.5, *)
@MainActor
func disconnected_swap_isSending() {
  var d = Disconnected(NonSendableKlass())
  let y = NonSendableKlass()
  d.swap(newValue: y)
  // expected-error @-1 {{sending 'y' risks causing data races}}
  // expected-note @-2 {{'y' used after being passed as a 'sending' parameter}}
  useValue(y) // expected-note {{access can happen concurrently}}
}

// `withValue`'s closure parameter is `inout sending Value`. That alone
// prevents the closure from returning a value that aliases into the wrapped
// value's storage: such an alias would propagate into the caller's region
// and leave the wrapped storage with cross-region references at closure
// exit, violating the `sending` invariant. The closure may still return
// values constructed independently of the wrapped value (Sendable results,
// fresh `Disconnected` wrappers, or values captured from the caller's
// region).

final class Inner {}
final class Outer {
  var inner = Inner()
}

// Returning a reference that aliases the wrapped value's storage is
// rejected. Without this rejection, the caller would hold a non-Sendable
// alias into the wrapped storage, with no way for the wrapper to know.
@available(SwiftStdlib 6.5, *)
@MainActor
func disconnected_withValue_rejectsAliasedReturn() {
  var wrapper = Disconnected(Outer())
  let leaked: Inner = wrapper.withValue { outer in
    return outer.inner
    // expected-error @-1 {{'outer.inner' cannot be returned}}
    // expected-note @-2 {{returning 'outer.inner' risks concurrent access to 'inout sending' parameter 'outer' as caller assumes 'outer' is not actor-isolated and result is main actor-isolated}}
  }
  useValue(leaked)
}

// Wrapping the aliased reference in `Disconnected` does not bypass the
// check: `Disconnected.init` itself takes `sending Value`, so the aliased
// `outer.inner` is rejected at the wrapper-construction boundary instead.
@available(SwiftStdlib 6.5, *)
@MainActor
func disconnected_withValue_rejectsAliasedReturnInWrapper() {
  var wrapper = Disconnected(Outer())
  let leaked: Disconnected<Inner> = wrapper.withValue { outer in
    return Disconnected(outer.inner)
    // expected-error @-1 {{sending 'outer.inner' risks causing data races}}
    // expected-note @-2 {{'outer.inner' used after being passed as a 'sending' parameter; Later uses could race}}
    // expected-note @-3 {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
  }
  _ = leaked
}

// Returning a freshly constructed `Disconnected` from the closure is
// allowed: the wrapped inner value is in its own disconnected region,
// independent of the surrounding `outer`. The caller can then unwrap it
// and use the result.
@available(SwiftStdlib 6.5, *)
@MainActor
func disconnected_withValue_allowsFreshDisconnectedReturn() {
  var wrapper = Disconnected(Outer())
  let result: Disconnected<Inner> = wrapper.withValue { _ in
    return Disconnected(Inner())
  }
  useValue(result.take())
}

// Returning a value tied to the caller's isolation region is permitted.
// This is the expressiveness advantage of leaving `Return` non-sending on
// `withValue`: the closure can hand back a result derived from caller-side
// state (here, the captured `callerOwned`) without anyone having to wrap
// it in `Disconnected`. If `Return` were required to be `sending`, the
// captured `callerOwned` would belong to the MainActor's region and
// would be rejected at the closure's return boundary, forcing callers
// to wrap or restructure even when they have no intent to ferry the
// value across an isolation boundary.
@available(SwiftStdlib 6.5, *)
@MainActor
func disconnected_withValue_allowsCallerRegionReturn() {
  var wrapper = Disconnected(Outer())
  let callerOwned = Inner()

  let returned: Inner = wrapper.withValue { _ in
    return callerOwned
  }

  // Both `callerOwned` and `returned` are the same reference, both still
  // in MainActor's region. The wrapper continues to hold its own,
  // independently disconnected `Outer`.
  useValue(callerOwned)
  useValue(returned)
}
