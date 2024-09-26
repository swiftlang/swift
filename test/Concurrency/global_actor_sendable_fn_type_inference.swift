// RUN: %target-typecheck-verify-swift -strict-concurrency=complete -disable-availability-checking -enable-upcoming-feature RegionBasedIsolation -enable-upcoming-feature GlobalActorIsolatedTypesUsability

// REQUIRES: concurrency
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability
// REQUIRES: swift_feature_RegionBasedIsolation

func inferSendableFunctionType() {
  let closure: @MainActor () -> Void = {}

  Task {
    await closure() // okay
  }
}

class NonSendable {}

func allowNonSendableCaptures() {
  let nonSendable = NonSendable()
  let _: @MainActor () -> Void = {
    let _ = nonSendable // okay
  }
}
