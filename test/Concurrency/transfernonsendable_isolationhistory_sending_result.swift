// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -parse-as-library -sil-region-isolation-emit-isolation-history -verify %s -o /dev/null

// REQUIRES: concurrency

// Swift-source coverage for isolation-history notes on the
// AssignNeverSendableIntoSendingResult diagnostic, which fires when a value
// whose region is not disconnected is assigned into an indirect 'sending'
// result. The caller expects such a result to arrive in its own region, so the
// notes explain which merge tied it to someone else's.
//
// The diagnostic needs an *indirect* sending result, so every function here is
// generic in its return type. The task-isolated parameter is the isolated
// source: a nonisolated function's non-sending arguments are task-isolated.
//
// Primary diagnostics use empty-body matchers; the chain notes have their full
// text matched, since they are what is under test.

////////////////////////////////////////////////////////////////////////////////
// Suppression — the returned value is *itself* task-isolated rather than having
// been merged into an isolated region, so there is no chain to explain and no
// history note should fire.
////////////////////////////////////////////////////////////////////////////////

func suppress_direct<T>(_ t: T) -> sending T {
  return t // expected-warning {{}} expected-note {{}}
}

////////////////////////////////////////////////////////////////////////////////
// One hop — a local picks up the task-isolated parameter's region, then is
// returned. One terminal note names the isolated source.
////////////////////////////////////////////////////////////////////////////////

func one_hop<T>(_ t: T) -> sending T {
  // expected-note@+1 {{'y' is connected to 't' which is accessible to code in the current isolation context}}
  let y = t
  return y // expected-warning {{}} expected-note {{}}
}

////////////////////////////////////////////////////////////////////////////////
// Two hops — through a named intermediate. Both links are reported.
////////////////////////////////////////////////////////////////////////////////

func two_hop<T>(_ t: T) -> sending T {
  // expected-note@+1 {{'y' is connected to 't' which is accessible to code in the current isolation context}}
  let y = t
  let z = y // expected-note {{'z' is connected to 'y'}}
  return z // expected-warning {{}} expected-note {{}}
}

////////////////////////////////////////////////////////////////////////////////
// CFG diamond — only the 'if' arm merges the returned local into the isolated
// region; the 'else' arm assigns a fresh value.
//
// FIXME: Two defects are pinned here rather than smoothed over. Both are
// specific to this diagnostic's shape -- the one-hop and two-hop cases above
// are correct -- and both need the join, so they do not reproduce on a
// straight-line function.
//
//  1. The notes name '$return_value', which is the SIL name of the indirect
//     result and not anything the user wrote. A user-facing note should never
//     print it. It appears because the out parameter is in the merged region, so
//     the walk's pairwise question splits through it, giving y -> $return_value
//     -> t instead of y -> t.
//
//  2. The notes are attributed to the *wrong arm*: they land on the 'else' arm's
//     assignment, which never touched 't', instead of the 'if' arm's 'y = t'.
//     Verified at the SIL level -- bb1 is the arm holding 'copy_addr %1 to
//     [init]' for the task-isolated argument, bb2 is the fresh assignment, and
//     the notes come out attributed to bb2. The merge being explained is one the
//     dataflow join performed, which has no sequence boundary in the joining
//     block, so its notes stay pending and are located only once the walk enters
//     a predecessor -- and the located boundary is not required to come from the
//     predecessor that actually answered the question. That is
//     prepareStatesForPredecessors' candidate filter, which README.md §5 flags as
//     the part of the walk most likely to misbehave on a non-apply anchor.
//
// When either is fixed this case should instead report, on the 'y = t' line:
//   'y' is connected to 't' which is accessible to code in the current isolation context
////////////////////////////////////////////////////////////////////////////////

func diamond<T>(_ t: T, _ makeFresh: () -> T, _ cond: Bool) -> sending T {
  var y = makeFresh()
  if cond {
    y = t
  } else {
    // expected-note@+2 {{'y' is connected to '$return_value'}}
    // expected-note@+1 {{'$return_value' is connected to 't' which is accessible to code in the current isolation context}}
    y = makeFresh()
  }
  return y // expected-warning {{}} expected-note {{}}
}
