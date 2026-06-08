// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -target %target-swift-5.1-abi-triple -verify -verify-additional-prefix named- %s -o /dev/null -parse-as-library -enable-upcoming-feature GlobalActorIsolatedTypesUsability -enable-experimental-feature FlowIsolationGlobalActor
// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -target %target-swift-5.1-abi-triple -Xllvm -sil-regionbasedisolation-force-use-of-typed-errors -verify -verify-additional-prefix bare- %s -o /dev/null -parse-as-library -enable-upcoming-feature GlobalActorIsolatedTypesUsability -enable-experimental-feature FlowIsolationGlobalActor

// REQUIRES: concurrency
// REQUIRES: asserts
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability
// REQUIRES: swift_feature_FlowIsolationGlobalActor

// This file exercises the *shape* of "incompatible region merge" diagnostics
// produced by the region-isolation analysis. The points being tested:
//
//   1. The primary diagnostic does not depend on value-name recovery. Even
//      under -sil-regionbasedisolation-force-use-of-typed-errors (which
//      suppresses name inference), every merge that the analysis understands
//      still produces a real primary -- never the "unknown pattern" fallback.
//      The two RUN lines exercise both name-recovery paths.
//
//   2. Value notes are anchored at each value's *defining* location with
//      column precision. The bare-note assertions pin the column explicitly
//      so that any regression in note-loc recovery (the SourceLoc collapsing
//      back to the merge site, or to the func keyword for SILFunctionArgument)
//      trips the verifier.
//
//   3. The task-isolated side renders as "code in the current isolation context" instead
//      of "task-isolated code", matching the rest of the diagnostic suite.
//
//   4. The isolatedfn primary has two variants: a named form that embeds the
//      source value's name (default) and an unnamed form ("passing value to
//      ...") used when name inference fails (typed-errors flag).
//
// Coverage matrix
// ---------------
//
// Each emitter in IncompatibleRegionMergeDiagnosticEmitter renders a primary
// from one of five DiagnosticsSIL.def entries and (zero or more) per-value
// notes from one of two note diagnostics. The matrix below records, per
// emitter, which RegionMergeReason dispatches to it, which scenarios in this
// file exercise it, and which isolation-string variants
// (cross-actor vs "code in the current isolation context") are covered. The unknown-pattern
// fallback companion lives in transfernonsendable_merge_region_checker_failures.swift.
//
//   emitter / primary diag                  | reason(s)                    | covered by                  | iso variants
//   -----------------------------------------+------------------------------+-----------------------------+---------------
//   emitNonisolatedFunction                  | NonisolatedFunction          | TwoIsolatedFields           | actor <-> actor
//     rbi_merge_failure_nonisolatedfn        |                              |                             |
//   emitNonisolatedFunction -> emitUnknown   | NonisolatedFunction          | IndirectCallee              | actor <-> actor
//     rbi_merge_failure_unknown              |   (no recoverable callee)    |                             |
//   emitUnknown                              | NonisolatedClosure, Builtin, | --- not reachable from      | ---
//     rbi_merge_failure_unknown              |   Unknown                    |   Swift surface (SIL only)  |
//   emitAssign                               | Assign                       | CastingActor.f (return q),  | actor <-> actor,
//     rbi_merge_failure_assign               |                              |   TupleMerge, TaskAssignTest|   task <-> actor
//   emitIsolatedFunction (named)             | ActorIntroducingInst         | TaskIsolatedSelfTest (named)| task <-> actor
//     rbi_merge_failure_isolatedfn_named     |   (ApplySite, name found)    |                             |
//   emitIsolatedFunction (bare)              | ActorIntroducingInst         | TaskIsolatedSelfTest (bare) | task <-> actor
//     rbi_merge_failure_isolatedfn           |   (ApplySite, no name)       |                             |
//   emitCast                                 | Cast, ActorIntroducingInst   | CastingActor.f (c as? P)    | actor <-> actor
//     rbi_merge_failure_cast                 |   (non-ApplySite)            |                             |
//
//   per-value note                           | when used                    | covered by                  |
//   -----------------------------------------+------------------------------+-----------------------------+
//   rbi_merge_failure_value_note_named       | name inference succeeded     | all named-prefix scenarios  |
//   rbi_merge_failure_value_note_bare        | name inference failed        | all bare-prefix scenarios   |
//
// Known coverage gaps (not reachable cleanly from the Swift surface; would
// need .sil tests to exercise):
//
//   - emitUnknown via NonisolatedClosure / Builtin reasons. The Sema layer
//     either rejects the cross-domain construction up front or routes the PA
//     through translateIsolatedPartialApply, so neither reason fires.
//   - emitNonisolatedFunction with task-isolated source. Sema blocks the
//     natural patterns (cross-actor property read from task context).
//   - emitCast with task-isolated source. Same reason as above.

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {} // expected-note 2 {{class 'NonSendableKlass' does not conform to the 'Sendable' protocol}}

@globalActor
actor CustomActor {
  static let shared = CustomActor()
}

func mergeValues<T>(_ x: T, _ y: NonSendableKlass) {}

/////////////////////////////////////////////////////////////////
// MARK: nonisolated callee + two differently-isolated arguments
/////////////////////////////////////////////////////////////////

// Two properties of differing isolations passed to a nonisolated generic.
// Each cross-domain property access produces a per-value note anchored at the
// projection that contributes the leaf component of the inferred name --
// e.g. `customField!` and `field2!` -- not at the enclosing `init` keyword.
@MainActor
struct TwoIsolatedFields {
  nonisolated(unsafe) var field: NonSendableKlass? = nil
  var field2: NonSendableKlass? = nil
  @CustomActor var customField: NonSendableKlass? = nil
  init() {
    mergeValues(field!, customField!)
    mergeValues(field2!, customField!) // expected-warning {{passing arguments to global function 'mergeValues' could allow for references between values exposed to global actor 'CustomActor'-isolated code and main actor-isolated code risking data races}}
    // expected-named-note @-1:26 {{'self.customField' is exposed to global actor 'CustomActor'-isolated code}}
    // expected-bare-note @-2:26 {{value is exposed to global actor 'CustomActor'-isolated code}}
    // expected-named-note @-3:23 {{'self.field2' is exposed to main actor-isolated code}}
    // expected-bare-note @-4:23 {{value is exposed to main actor-isolated code}}
  }
}

/////////////////////////////////////////////////////////////////////
// MARK: isolated callee + task-isolated source ("code in the current isolation context")
/////////////////////////////////////////////////////////////////////

// A nonisolated method on a non-Sendable class invoking a @MainActor getter
// on `self`. `self` is task-isolated -- the merge primary should render this
// side as "code in the current isolation context". The `self` value note anchors at the
// `func call` token (col 8) because `self` is a SILFunctionArgument and we
// resolve to the parameter decl loc.
//
// Returning a Sendable type (Int) from the @MainActor getter avoids any
// follow-up assignment-merge so the diagnostics here focus on the isolatedfn
// case.
final class TaskIsolatedSelfTest { // expected-note {{class 'TaskIsolatedSelfTest' does not conform to the 'Sendable' protocol}}
  func call() async { // expected-named-note@:8 {{'self' is exposed to code in the current isolation context}}
                      // expected-bare-note@-1:8 {{value is exposed to code in the current isolation context}}
    _ = await x
    // expected-named-warning @-1 {{passing 'self' to main actor-isolated getter for property 'x' could allow for references between values exposed to code in the current isolation context and main actor-isolated code risking data races}}
    // expected-bare-warning @-2 {{passing value to main actor-isolated getter for property 'x' could allow for references between values exposed to code in the current isolation context and main actor-isolated code risking data races}}
    // expected-warning @-3 {{non-Sendable type 'TaskIsolatedSelfTest' cannot be sent into main actor-isolated context in call to property 'x'; this is an error in the Swift 6 language mode}}
  }

  @MainActor
  var x: Int { 0 }
}

/////////////////////////////////////////////////////////////////
// MARK: nonisolatedfn falls through to emitUnknown (indirect callee)
/////////////////////////////////////////////////////////////////

// Calling through a function-typed local loses ApplySite::getCalleeDeclRef(),
// so emitNonisolatedFunction can not emit its named primary. Instead of
// degrading to the "unknown pattern" fallback (which is reserved for analysis
// failures), the emitter falls through to emitUnknown(), which renders the
// generic "executing operation could allow ..." primary while still preserving
// per-value notes for both sides of the merge.
@MainActor
struct IndirectCallee {
  var field2: NonSendableKlass? = nil
  @CustomActor var customField: NonSendableKlass? = nil

  init() {
    let f: (NonSendableKlass, NonSendableKlass) -> Void = { _, _ in }
    f(field2!, customField!) // expected-warning {{executing operation could allow for references between values exposed to global actor 'CustomActor'-isolated code and main actor-isolated code risking data races}}
    // expected-named-note @-1:16 {{'self.customField' is exposed to global actor 'CustomActor'-isolated code}}
    // expected-named-note @-2:7 {{'self.field2' is exposed to main actor-isolated code}}
    // expected-bare-note @-3:16 {{value is exposed to global actor 'CustomActor'-isolated code}}
    // expected-bare-note @-4:7 {{value is exposed to main actor-isolated code}}
  }
}

/////////////////////////////////////////////////////////////////
// MARK: tuple constructor merging cross-domain values
/////////////////////////////////////////////////////////////////

// A tuple constructor with cross-domain operands lowers to a translateSILAssign
// (TupleInst is CONSTANT_TRANSLATION AssignDirect), so the merge fires through
// emitAssign — distinct from the explicit `=` assign covered below by the cast
// scenario. This exercises the same primary but reached via a different
// instruction shape.
@MainActor
struct TupleMerge {
  var field2: NonSendableKlass? = nil
  @CustomActor var customField: NonSendableKlass? = nil

  init(_ tag: Int) {
    let pair = (field2!, customField!) // expected-warning {{assignment could allow for references between values exposed to global actor 'CustomActor'-isolated code and main actor-isolated code risking data races}}
    // expected-named-note @-1:26 {{'self.customField' is exposed to global actor 'CustomActor'-isolated code}}
    // expected-named-note @-2:17 {{'self.field2' is exposed to main actor-isolated code}}
    // expected-bare-note @-3:26 {{value is exposed to global actor 'CustomActor'-isolated code}}
    // expected-bare-note @-4:17 {{value is exposed to main actor-isolated code}}
    _ = pair
  }
}

/////////////////////////////////////////////////////////////////
// MARK: assign — task-isolated source side ("code in the current isolation context")
/////////////////////////////////////////////////////////////////

// AsyncStream<Element>.init(_:) takes a @Sendable closure executed in a task
// context. Reading `self.stored` (MainActor-iso) from inside the closure and
// implicitly yielding it merges a MainActor-iso value into the task-iso
// continuation, triggering emitAssign with the task-iso rendering on one side.
//
// The other warnings on this line are the Sema-side cross-actor access and
// non-Sendable exit diagnostics — verified here so any regression that
// suppresses or duplicates them trips the verifier.
@MainActor
final class TaskAssignTest {
  var stored: NonSendableKlass = NonSendableKlass()
  func makeStream() -> AsyncStream<NonSendableKlass> {
    AsyncStream<NonSendableKlass> {
      self.stored // expected-warning {{assignment could allow for references between values exposed to code in the current isolation context and main actor-isolated code risking data races}}
      // expected-named-note @-1 {{'self.stored' is exposed to main actor-isolated code}}
      // expected-bare-note @-2 {{value is exposed to main actor-isolated code}}
      // expected-warning @-3 {{main actor-isolated property 'stored' cannot be accessed from outside of the actor; this is an error in the Swift 6 language mode}}
      // expected-warning @-4 {{non-Sendable type 'NonSendableKlass' of property 'stored' cannot exit main actor-isolated context; this is an error in the Swift 6 language mode}}
    }
  }
}

//////////////////////////////////
// MARK: cast across domains
//////////////////////////////////

protocol NonSendableProtocol {}

// An actor whose stored property is @MainActor-isolated. Reading the property
// drags a main-actor-isolated value into a self-isolated context; the cast
// then merges into a self-isolated type, and `return` merges into the
// self-isolated $return_value.
//
// This case validates two emitter shapes:
//   - emitCast: produces "casting value to type ..." with a single value
//     note for the source.
//   - emitAssign: produces "assignment could allow for references between ..."
//     with a value note for the named source value (the $return_value side
//     has no source loc and is suppressed by emitMergeRegionValueNote).
actor CastingActor {
  @MainActor var stored: NonSendableKlass?

  func f() async -> NonSendableProtocol {
    guard let c = await stored else { fatalError() }
    // expected-warning @-1 {{non-Sendable type 'NonSendableKlass?' of property 'stored' cannot exit main actor-isolated context; this is an error in the Swift 6 language mode}}

    // The note on `c` anchors at the *use* in the cast (col 21) because the
    // SIL value being analyzed is the operand of the cast, not the original
    // `let c` binding.
    guard let q = c as? NonSendableProtocol else { fatalError() }
    // expected-warning @-1 {{casting value to type 'any NonSendableProtocol' could allow for references between values exposed to main actor-isolated code and 'self'-isolated code risking data races}}
    // expected-named-note @-2:21 {{'c' is exposed to main actor-isolated code}}
    // expected-bare-note @-3:21 {{value is exposed to main actor-isolated code}}

    // The note on `q` anchors at q's defining loc above (col 15 of the
    // `guard let q = ...` line).
    return q
    // expected-warning @-1 {{assignment could allow for references between values exposed to main actor-isolated code and 'self'-isolated code risking data races}}
    // expected-named-note @-9:15 {{'q' is exposed to main actor-isolated code}}
    // expected-bare-note @-10:15 {{value is exposed to main actor-isolated code}}
  }
}
