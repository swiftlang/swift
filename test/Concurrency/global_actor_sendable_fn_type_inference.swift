// RUN: %target-typecheck-verify-swift -strict-concurrency=complete -target %target-swift-5.1-abi-triple -enable-upcoming-feature RegionBasedIsolation -enable-upcoming-feature GlobalActorIsolatedTypesUsability

// REQUIRES: concurrency
// REQUIRES: asserts

func inferSendableFunctionType() {
  let closure: @MainActor () -> Void = {}
  @MainActor func f() {}

  Task {
    await closure() // okay
    await f() // okay
  }
}

class NonSendable {}

func allowNonSendableCaptures() {
  let nonSendable = NonSendable()
  let _: @MainActor () -> Void = {
    let _ = nonSendable // okay
  }
}
