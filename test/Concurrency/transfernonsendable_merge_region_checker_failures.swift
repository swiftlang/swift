// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -target %target-swift-5.1-abi-triple -verify %s -o /dev/null -parse-as-library -enable-upcoming-feature GlobalActorIsolatedTypesUsability -enable-experimental-feature FlowIsolationGlobalActor

// REQUIRES: concurrency
// REQUIRES: asserts
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability
// REQUIRES: swift_feature_FlowIsolationGlobalActor

// This file is the *negative* companion to
// transfernonsendable_merge_region_diagnostics.swift. Each scenario below is a
// Swift-level pattern that hits the region-isolation checker's
// "regionbasedisolation_unknown_pattern" fallback rather than one of the
// shaped merge diagnostics. These cases represent real coverage holes in
// either the region analysis (one side's SILIsolationInfo is lost before the
// merge fires) or the emitter (the operation shape is not matched by any of
// the five specialised emitters).
//
// Pin them here so any future fix that makes the analysis recover --- and thus
// upgrades one of these warnings into a real shaped diagnostic --- trips the
// verifier and forces an explicit decision about the new wording.

class NonSendableKlass {}

@globalActor
actor CustomActor {
  static let shared = CustomActor()
}

///////////////////////////////////////////////////////////////////
// MARK: ternary phi merging cross-domain non-Sendable operands
///////////////////////////////////////////////////////////////////

// `pick ? a : b` lowers to a SIL phi where each predecessor stores a
// cross-domain value into the bb argument. The phi merge runs through
// translateSILPhi -> translateSILMultiAssign(reason = Unknown) ->
// emitUnknown(). emitUnknown requires both src and dst isolations to be
// recoverable; here the dst (the phi-result) carries no isolation override and
// the merge falls through to emitUnknownPatternError().
//
// Expected fix: translateSILPhi should propagate one side's isolation onto the
// phi result so emitUnknown can render its generic
// "executing operation could allow ..." primary. When that lands, the
// expectation below will flip to that wording.
@MainActor
struct TernaryPhi {
  var field2: NonSendableKlass? = nil
  @CustomActor var customField: NonSendableKlass? = nil

  init(_ pick: Bool) {
    let chosen = pick ? field2! : customField! // expected-warning {{pattern that the region-based isolation checker does not understand how to check. Please file a bug}}
    _ = chosen
  }
}
