// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -parse-as-library -sil-region-isolation-emit-isolation-history -verify %s -o /dev/null

// REQUIRES: concurrency

// Swift-source coverage for isolation-history notes on the
// InOutSendingNotDisconnectedAtExit diagnostic. The companion
// transfernonsendable_isolationhistory_inout_sending_not_disconnected.sil
// covers the per-merge-kind axes directly; this file exists so we know the
// chain survives real SILGen output, where the loads, temporaries and
// per-argument locations are the ones the compiler actually emits rather than
// ones we authored.
//
// The primary diagnostic uses an empty-body matcher; the chain notes have
// their full text matched, since they are what is under test.
//
// FIXME: A chain whose isolated source is an actor's stored property read
// directly into a local is truncated: it explains its first hop and then stops,
// never naming the isolated value or where the value entered the isolated
// region. 'twoHop' below pins that truncation so the bug does not change
// silently; the note it *should* also produce is spelled out at that case.
//
// The cause is two individually-reasonable heuristics colliding. Naming a call
// result after its callee deliberately exempts accessors, because in source the
// user wrote a property rather than a call (VariableNameUtils.cpp: "An
// accessor's name is its storage's name"), so the getter's result is named
// after the variable it initializes -- 'z'. The walk's isSameUserValue then
// sees both ends of the merge named 'z' and drops the link as an artifact of
// lowering, which is what that rule is for: real SILGen temporaries. Here it
// discards a semantically distinct value.
//
// This predates isolation-history notes on this diagnostic; it equally
// truncates the already-shipping SentNeverSendable notes, where 'let z = ns;
// await send(z)' inside an actor emits no history note at all. Verified against
// a build of the tree before this series. Reading a global actor-isolated
// variable does not collapse this way, so 'globalTwoHop' below does get its
// terminal note -- the asymmetry between those two cases is the bug's
// signature. Likely fix: under Flag::NameCallResultAfterCallee, name an
// accessor result after its storage ('self.ns') rather than after the local,
// which is already what the no-local case prints.
//
// One further shape is pinned, and is intended behavior rather than a bug: the
// chain reads outward from the parameter, so the note naming the isolated
// source is the *last* one, and on a multi-hop chain the notes are reported in
// discovery order rather than reading order.

class NonSendableKlass {}

@MainActor var globalKlass = NonSendableKlass()

////////////////////////////////////////////////////////////////////////////////
// One hop — the 'inout sending' parameter is assigned the isolated value
// directly, so one terminal note explains the whole chain.
////////////////////////////////////////////////////////////////////////////////

actor MyActor {
  var ns = NonSendableKlass()

  func oneHop(_ x: inout sending NonSendableKlass) {
    // expected-note@+1 {{'x' is connected to 'self.ns' which is accessible to 'self'-isolated code}}
    x = ns
  } // expected-warning {{}} expected-note {{}}

  // FIXME: Truncated chain -- see the FIXME in the header comment. This reports
  // only the first hop and never explains how 'z' became 'self'-isolated, so
  // the reader is told 'x' is connected to a local that is itself unexplained.
  // When the accessor-naming bug is fixed, this case should additionally
  // produce, on the 'let z = ns' line:
  //   'z' is connected to 'self.ns' which is accessible to 'self'-isolated code
  // which is what the equivalent global-actor case ('globalTwoHop') already
  // produces today.
  func twoHop(_ x: inout sending NonSendableKlass) {
    let z = ns
    x = z // expected-note {{'x' is connected to 'z'}}
  } // expected-warning {{}} expected-note {{}}
}

@MainActor
func globalOneHop(_ x: inout sending NonSendableKlass) {
  // expected-note@+1 {{'x' is connected to 'globalKlass' which is accessible to main actor-isolated code}}
  x = globalKlass
} // expected-warning {{}} expected-note {{}}

////////////////////////////////////////////////////////////////////////////////
// Two hops — routed through a named local. Unlike the actor case above, a
// global actor-isolated variable does not collapse onto the local's name, so
// both links are reported.
////////////////////////////////////////////////////////////////////////////////

@MainActor
func globalTwoHop(_ x: inout sending NonSendableKlass) {
  // expected-note@+1 {{'z' is connected to 'globalKlass' which is accessible to main actor-isolated code}}
  let z = globalKlass
  x = z // expected-note {{'x' is connected to 'z'}}
} // expected-warning {{}} expected-note {{}}

////////////////////////////////////////////////////////////////////////////////
// CFG diamond — only one arm merges the parameter into the isolated region.
// The note must land in that arm; the other arm assigns a fresh disconnected
// value and must produce none.
////////////////////////////////////////////////////////////////////////////////

@MainActor
func diamond(_ x: inout sending NonSendableKlass, _ cond: Bool) {
  if cond {
    // expected-note@+1 {{'x' is connected to 'globalKlass' which is accessible to main actor-isolated code}}
    x = globalKlass
  } else {
    let fresh = NonSendableKlass()
    x = fresh
  }
} // expected-warning {{}} expected-note {{}}
