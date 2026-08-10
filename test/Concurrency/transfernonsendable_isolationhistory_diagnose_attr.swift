// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -parse-as-library -verify %s -o /dev/null

// REQUIRES: concurrency

// Verifies that the `@diagnose(RegionIsolationIsolationHistory, as: warning)`
// attribute on a function opts that function in to isolation-history note
// emission, even when the `-sil-region-isolation-emit-isolation-history`
// frontend flag is not passed.
//
// Conversely, a function with no attribute should NOT emit isolation-history
// notes. The `no_attr` functions below validate this by omitting the
// reachability-note expectation, so verify-mode fails if it fires.
//
// There is one opted-in / no-attribute pair per diagnostic that emits these
// notes, since each consumer gates independently.

@MainActor func transferToMain<T>(_ t: T) async {}

class NS {}

////////////////////////////////////////////////////////////////////////////////
// Function with `@diagnose(RegionIsolationIsolationHistory, as: warning)` —
// history notes SHOULD fire.
////////////////////////////////////////////////////////////////////////////////

@diagnose(RegionIsolationIsolationHistory, as: warning)
func opted_in(_ x: NS) async {
  // expected-note@+1{{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  let y = x
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Function with no `@diagnose` attribute — no isolation-history note should
// fire (the LLVM flag is the only other opt-in, and it isn't set here). The
// primary warning still fires; only the history note is suppressed.
////////////////////////////////////////////////////////////////////////////////

func no_attr(_ x: NS) async {
  let y = x
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// The same pair for the InOutSendingNotDisconnectedAtExit diagnostic. Each
// diagnostic that grew history notes needs its own gate row: the recording gate
// and the consuming gate are separate conditions that have to agree, and a
// consumer that forgets to check is only caught on a path some test walks.
////////////////////////////////////////////////////////////////////////////////

@MainActor var globalNS = NS()

@MainActor
@diagnose(RegionIsolationIsolationHistory, as: warning)
func inout_sending_opted_in(_ x: inout sending NS) {
  // expected-note@+1 {{'x' is connected to 'globalNS' which is accessible to main actor-isolated code}}
  x = globalNS
} // expected-warning {{'inout sending' parameter 'x' cannot be main actor-isolated at end of function}}
// expected-note @-1 {{main actor-isolated 'x' risks causing races in between main actor-isolated uses and caller uses since caller assumes value is not actor isolated}}

@MainActor
func inout_sending_no_attr(_ x: inout sending NS) {
  x = globalNS
} // expected-warning {{'inout sending' parameter 'x' cannot be main actor-isolated at end of function}}
// expected-note @-1 {{main actor-isolated 'x' risks causing races in between main actor-isolated uses and caller uses since caller assumes value is not actor isolated}}

////////////////////////////////////////////////////////////////////////////////
// The same pair for AssignNeverSendableIntoSendingResult.
////////////////////////////////////////////////////////////////////////////////

@diagnose(RegionIsolationIsolationHistory, as: warning)
func sending_result_opted_in<T>(_ t: T) -> sending T {
  // expected-note@+1 {{'y' is connected to 't' which is accessible to code in the current isolation context}}
  let y = t
  return y // expected-warning {{returning 'y' as a 'sending' result risks causing data races}}
  // expected-note @-1 {{returning 'y' risks causing data races since the caller assumes that 'y' can be safely sent to other isolation domains}}
}

func sending_result_no_attr<T>(_ t: T) -> sending T {
  let y = t
  return y // expected-warning {{returning 'y' as a 'sending' result risks causing data races}}
  // expected-note @-1 {{returning 'y' risks causing data races since the caller assumes that 'y' can be safely sent to other isolation domains}}
}

////////////////////////////////////////////////////////////////////////////////
// The same pair for InOutSendingReturned, using its connect request shape.
////////////////////////////////////////////////////////////////////////////////

@diagnose(RegionIsolationIsolationHistory, as: warning)
func inout_sending_returned_opted_in(_ x: inout sending NS, _ y: NS) -> sending NS {
  x = y // expected-note {{'y' is connected to 'x'}}
  return y // expected-warning 2 {{}} expected-note 2 {{}}
}

func inout_sending_returned_no_attr(_ x: inout sending NS, _ y: NS) -> sending NS {
  x = y
  return y // expected-warning 2 {{}} expected-note 2 {{}}
}
