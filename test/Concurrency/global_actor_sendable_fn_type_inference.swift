// RUN: %target-typecheck-verify-swift -strict-concurrency=complete -target %target-swift-5.1-abi-triple -enable-upcoming-feature GlobalActorIsolatedTypesUsability

// REQUIRES: concurrency
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability

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

func testLocalFunctionReference() {
  @MainActor func f() {}
  _ = f as any Sendable
}
