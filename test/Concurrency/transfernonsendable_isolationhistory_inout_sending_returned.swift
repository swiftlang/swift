// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -parse-as-library -sil-region-isolation-emit-isolation-history -verify %s -o /dev/null

// REQUIRES: concurrency

// Swift-source coverage for isolation-history notes on the InOutSendingReturned
// diagnostic, which fires at a function exit when the returned value is in the
// same region as an 'inout sending' parameter. The caller treats the two as
// independent regions, so it could send them to different isolation domains.
//
// This diagnostic is the first user of *both* request shapes:
//
//   - Returned the parameter itself -> nothing pairs up, so the question is why
//     the parameter's region is isolated (howDidBecomeIsolatedTo). The chain
//     terminates at the isolated value, "... which is accessible to <iso>".
//   - Returned some other value in the same region -> the question is how the
//     two came to share one (howDidBecomeConnectedTo). Such a chain has no
//     isolated terminus, so its last link is a plain 'a' is connected to 'b'.
//     That reads a little abruptly; a dedicated terminal diagnostic is a
//     deliberate follow-up, not an omission here.
//
// Primary diagnostics use empty-body matchers; chain notes have their full text
// matched. Several of these functions trip more than one diagnostic on the same
// line, and where a second diagnostic also carries a chain it is matched too --
// those extra chains belong to other emitters and are pinned so this file
// notices if they change.
//
// NOT COVERED, and why: README.md §5 asks for a suppression case where the
// element is directly isolated rather than merged. For the reach-isolation shape
// that element is the 'inout sending' parameter, and SILIsolationInfo::get
// returns getDisconnected() unconditionally for a sending argument ("Sending is
// always disconnected"), so it can never be directly isolated. For the connect
// shape, suppression would mean the two ends are in different regions -- but then
// the diagnostic does not fire at all. 'return_disconnected' below covers the
// nearest reachable thing and is the more valuable test anyway; see its comment.

class NS {}

@MainActor var globalNS = NS()

////////////////////////////////////////////////////////////////////////////////
// howDidBecomeIsolatedTo — returned the 'inout sending' parameter itself.
////////////////////////////////////////////////////////////////////////////////

@MainActor
func reach_one_hop(_ x: inout sending NS) -> sending NS {
  // expected-note@+1 2 {{'x' is connected to 'globalNS' which is accessible to main actor-isolated code}}
  x = globalNS
  return x // expected-warning 2 {{}} expected-note 2 {{}}
}

@MainActor
func reach_two_hop(_ x: inout sending NS) -> sending NS {
  // expected-note@+1 2 {{'z' is connected to 'globalNS' which is accessible to main actor-isolated code}}
  let z = globalNS
  x = z // expected-note 2 {{'x' is connected to 'z'}}
  return x // expected-warning 2 {{}} expected-note 2 {{}}
}

////////////////////////////////////////////////////////////////////////////////
// howDidBecomeConnectedTo — returned a different value sharing the parameter's
// region. Note the last link is a plain "is connected to": a connect chain has
// no isolated terminus.
////////////////////////////////////////////////////////////////////////////////

func connect_one_hop(_ x: inout sending NS, _ y: NS) -> sending NS {
  x = y // expected-note {{'y' is connected to 'x'}}
  return y // expected-warning 2 {{}} expected-note 2 {{}}
}

func connect_two_hop(_ x: inout sending NS, _ y: NS) -> sending NS {
  let z = y // expected-note {{'y' is connected to 'z'}}
  x = z // expected-note {{'z' is connected to 'x'}}
  return y // expected-warning 2 {{}} expected-note 2 {{}}
}

////////////////////////////////////////////////////////////////////////////////
// The guard from README.md §4.1, exercised.
//
// InOutSendingReturnedError's constructors default isolationInfo to {}, which is
// the Invalid lattice element, and emitNotes() would hand that to
// printForDiagnostics, which report_fatal_error()s on it. All four construction
// sites happen to pass an explicit isolation today, so the literal default is
// unreachable -- but this function reaches the reach-isolation shape down the
// *disconnected* path, which carries the weakest isolation the error can hold.
// shouldEmit() must decline it rather than format it: no chain, and above all no
// crash. If this ever starts printing a chain, the isolation being formatted
// needs re-checking.
////////////////////////////////////////////////////////////////////////////////

func return_disconnected(_ x: inout sending NS) -> sending NS {
  return x // expected-warning {{}} expected-note {{}}
}

////////////////////////////////////////////////////////////////////////////////
// CFG diamond — only the 'if' arm connects the returned value to the parameter.
// The note lands in that arm, and the 'else' arm produces none. (Unlike the
// diamond in transfernonsendable_isolationhistory_sending_result.swift, this one
// is located correctly: the merge being explained has a sequence boundary in the
// arm that performed it.)
////////////////////////////////////////////////////////////////////////////////

func diamond(_ x: inout sending NS, _ y: NS, _ cond: Bool) -> sending NS {
  if cond {
    x = y // expected-note {{'y' is connected to 'x'}}
  } else {
    let fresh = NS()
    x = fresh
  }
  return y // expected-warning 2 {{}} expected-note 2 {{}}
}

////////////////////////////////////////////////////////////////////////////////
// Two returns, i.e. the epilogue-phi / multi-finalValues path in emit(). That
// path can emit several primary diagnostics from one error, but the note emitter
// deliberately emits exactly one chain per error -- see the comment on
// emitIsolationHistoryNoteIfNeeded. The counts below pin what that produces so a
// change in the primaries does not silently multiply the notes.
////////////////////////////////////////////////////////////////////////////////

func multi_return(_ x: inout sending NS, _ y: NS, _ cond: Bool) -> sending NS {
  x = y
  if cond {
    return y // expected-warning {{}} expected-note {{}}
  }
  // expected-note@+3 {{'x' is connected to 'y' which is accessible to code in the current isolation context}}
  // expected-note@+2 2 {{'y' is connected to 'x'}}
  // expected-warning@+1 {{}} expected-note@+1 {{}}
  return y
} // expected-warning {{}} expected-note {{}}
