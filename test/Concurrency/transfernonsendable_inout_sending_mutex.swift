// RUN: %target-swift-frontend -emit-sil -swift-version 6 -disable-availability-checking -verify %s -o /dev/null

// This test verifies that the region isolation pass correctly detects the
// 'inout sending' return violation when using Mutex.withLock, which passes
// a closure taking an 'inout sending' parameter and returning via an
// indirect @out result.

// REQUIRES: concurrency
// REQUIRES: synchronization

import Synchronization

class NonSendableKlass {
  var name = "Fred"
}

// The fundamental pattern: Mutex.withLock { $0 } attempts to return the
// inout sending parameter directly, which would allow the caller to hold
// a reference that aliases the Mutex's protected state.
func testMutexWithLockReturnsParam() {
  let m = Mutex(NonSendableKlass())
  let _ = m.withLock { $0 } // expected-error {{'inout sending' parameter '$0' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter '$0' risks concurrent access as caller assumes '$0' and result can be sent to different isolation domains}}
}

// Explicit closure with named parameter.
func testMutexWithLockExplicitClosure() {
  let m = Mutex(NonSendableKlass())
  let _ = m.withLock { (state: inout sending NonSendableKlass) -> NonSendableKlass in
    return state // expected-error {{'inout sending' parameter 'state' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'state' risks concurrent access as caller assumes 'state' and result can be sent to different isolation domains}}
  }
}

// Returning a derived value from the mutex state.
func testMutexWithLockReturnsDerived() {
  let m = Mutex(NonSendableKlass())
  let _ = m.withLock { (state: inout sending NonSendableKlass) -> NonSendableKlass in
    let copy = state
    return copy // expected-error {{'copy' cannot be returned}}
    // expected-note @-1 {{returning 'copy' risks concurrent access to 'inout sending' parameter 'state' as caller assumes 'state' and result can be sent to different isolation domains}}
  }
}

// Safe pattern: extract a Sendable value from the mutex state.
func testMutexWithLockReturnsSendableValue() {
  let m = Mutex(NonSendableKlass())
  let name: String = m.withLock { (state: inout sending NonSendableKlass) -> String in
    return state.name // OK - String is Sendable
  }
  _ = name
}

func testMutexWithLockAssignsOverButStillReturnsIt() {
  let m = Mutex(NonSendableKlass())
  let _ = m.withLock { (state: inout sending NonSendableKlass) -> NonSendableKlass in
    state = NonSendableKlass()
    return state // expected-error {{'inout sending' parameter 'state' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'state' risks concurrent access as caller assumes 'state' and result can be sent to different isolation domains}}
  }
}

func testMutexWithLockAssignsToCapture() {
  let m = Mutex(NonSendableKlass())
  let m2 = NonSendableKlass()
  let _ = m.withLock { (state: inout sending NonSendableKlass) -> NonSendableKlass in
    state = m2
    return state // expected-error {{'inout sending' parameter 'state' cannot be task-isolated at end of function}}
    // expected-note @-1 {{task-isolated 'state' risks causing races in between task-isolated uses and caller uses since caller assumes value is not actor isolated}}
    // expected-error @-2 {{returning task-isolated 'state' as a 'sending' result risks causing data races}}
    // expected-note @-3 {{returning task-isolated 'state' risks causing data races since the caller assumes that 'state' can be safely sent to other isolation domains}}
  }
}
