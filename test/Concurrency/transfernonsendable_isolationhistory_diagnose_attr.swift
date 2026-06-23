// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -parse-as-library -verify %s -o /dev/null

// REQUIRES: concurrency

// Verifies that the `@diagnose(RegionIsolationIsolationHistory, as: warning)`
// attribute on a function opts that function in to isolation-history note
// emission for SentNeverSendable diagnostics, even when the
// `-sil-region-isolation-emit-isolation-history` frontend flag is not passed.
//
// Conversely, a function with no attribute should NOT emit isolation-history
// notes. The `no_attr` function below validates this by omitting the
// reachability-note expectation, so verify-mode fails if it fires.

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
