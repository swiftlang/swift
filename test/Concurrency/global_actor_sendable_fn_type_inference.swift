// RUN: %target-typecheck-verify-swift -strict-concurrency=complete -disable-availability-checking -enable-upcoming-feature RegionBasedIsolation -enable-experimental-feature GlobalActorIsolatedTypesUsability

// REQUIRES: concurrency
// REQUIRES: asserts

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
