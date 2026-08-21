// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -target %target-swift-5.1-abi-triple -parse-as-library -enable-upcoming-feature GlobalActorIsolatedTypesUsability -sil-region-isolation-emit-isolation-history -verify %s -o /dev/null

// REQUIRES: concurrency
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability

// Swift-source coverage for isolation-history notes on the
// IncompatibleRegionMerge diagnostic, which fires when the analysis tries to
// merge two regions whose isolation domains are incompatible.
//
// This is the only diagnostic in the isolation-history series whose *existing*
// output changes. It already emitted a shallow per-side note -- "X is exposed to
// <iso> code" -- which names a value and its isolation but not how it got there.
// The chain is the deep version of exactly that, so where a chain is available it
// replaces the shallow note rather than being emitted alongside it; where there
// is no chain, the shallow note stays. The three cases below pin all three
// outcomes, and the mixed one is what proves the choice is made per side.
//
// Note the severity: this diagnostic warns until the *future* language mode, not
// v6, so these are warnings even under -swift-version 6.
//
// Two requests per error, one per side, each rewinding its own copy of the
// snapshot. The snapshot is taken *before* the failed merge -- the error is
// raised ahead of the assign/merge that would join the regions -- so the two
// regions are still separate in it and each side has its own independent
// history. That is what makes two independent walks meaningful.

class NS {}

func mergeValues<T, U>(_ t: T, _ u: U) {}

actor CustomActorInstance {}
@globalActor struct CustomActor { static let shared = CustomActorInstance() }

////////////////////////////////////////////////////////////////////////////////
// Both sides have chains — the behavior change. Each side is explained by its
// own chain, and *neither* shallow "is exposed to" note is emitted. The absence
// is the point, so it is asserted by omission: -verify fails on any unexpected
// note.
//
// FIXME: The two chains are both anchored on the second local's line rather than
// each on the line that created it -- 'a' is bound on the line above but its note
// lands with 'b'. Pinned as-is. This is the same family of mislocation FIXME'd in
// transfernonsendable_isolationhistory_sending_result.swift: a note's location
// comes from the sequence boundary the walk reaches, which is not required to be
// the one that performed the merge being described.
////////////////////////////////////////////////////////////////////////////////

@MainActor
struct BothChained {
  var mainField: NS? = nil
  @CustomActor var customField: NS? = nil
  init() {
    let a = mainField!
    // expected-note@+2 {{'b' is connected to 'self.customField' which is accessible to global actor 'CustomActor'-isolated code}}
    // expected-note@+1 {{'a' is connected to 'self.mainField' which is accessible to main actor-isolated code}}
    let b = customField!
    mergeValues(a, b) // expected-warning {{}}
  }
}

////////////////////////////////////////////////////////////////////////////////
// Mixed — one side has a chain, the other keeps its shallow note. This is the
// case that proves the per-side choice is driven by whether the walk actually
// emitted anything, rather than by whether history recording is on: recording is
// on for the whole function here, yet 'self.customField' is directly isolated, so
// it has no chain and must keep the old note.
////////////////////////////////////////////////////////////////////////////////

@MainActor
struct Mixed {
  var mainField: NS? = nil
  @CustomActor var customField: NS? = nil
  init() {
    let a = mainField!
    // expected-note@+2 {{'self.customField' is exposed to global actor 'CustomActor'-isolated code}}
    // expected-note@+1 {{'a' is connected to 'self.mainField' which is accessible to main actor-isolated code}}
    mergeValues(a, customField!) // expected-warning {{}}
  }
}

////////////////////////////////////////////////////////////////////////////////
// Neither side has a chain — both values are directly isolated, so the output is
// identical to what shipped before this change: two shallow notes, no chains.
////////////////////////////////////////////////////////////////////////////////

@MainActor
struct Neither {
  var mainField: NS? = nil
  @CustomActor var customField: NS? = nil
  init() {
    // expected-note@+2 {{'self.customField' is exposed to global actor 'CustomActor'-isolated code}}
    // expected-note@+1 {{'self.mainField' is exposed to main actor-isolated code}}
    mergeValues(mainField!, customField!) // expected-warning {{}}
  }
}

////////////////////////////////////////////////////////////////////////////////
// Per-reason coverage. Each RegionMergeReason routes to its own sub-emitter with
// its own note call sites, and three of the five swap the two sides to put the
// task-isolated one first -- a swapped isolation paired with an unswapped element
// would explain the wrong region, so the isolation strings below are what catch a
// mis-paired call site.
//
// The cases above cover NonisolatedFunction (the mergeValues calls). Below:
// Assign, and ActorIntroducingInst on its src-only path. Cast and Unknown reach
// emitSideNote through the same helper and are covered with shallow notes by
// transfernonsendable_merge_region_diagnostics.swift, which this change leaves
// byte-identical.
////////////////////////////////////////////////////////////////////////////////

// Assign: a tuple joining two differently-isolated locals. Both sides chained.
@MainActor
struct AssignReason {
  var mainField: NS? = nil
  @CustomActor var customField: NS? = nil
  init() {
    let a = mainField!
    // expected-note@+2 {{'b' is connected to 'self.customField' which is accessible to global actor 'CustomActor'-isolated code}}
    // expected-note@+1 {{'a' is connected to 'self.mainField' which is accessible to main actor-isolated code}}
    let b = customField!
    let t = (a, b) // expected-warning {{}}
    _ = t
  }
}

// ActorIntroducingInst, src-only: a main-actor getter reached through a
// task-isolated 'self'. 'self' is directly task-isolated, so this exercises the
// src-only call site taking its shallow fallback.
final class TaskIsolatedSelf { // expected-note {{class 'TaskIsolatedSelf' does not conform to the 'Sendable' protocol}}
  @MainActor var x: Int { 0 }
  func call() async { // expected-note {{'self' is exposed to code in the current isolation context}}
    _ = await x // expected-warning {{}} expected-warning {{}}
  }
}
