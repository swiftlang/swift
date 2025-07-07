// RUN: %target-typecheck-verify-swift -swift-version 6 -enable-upcoming-feature NonisolatedNonsendingByDefault -parse-as-library

// REQUIRES: asserts
// REQUIRES: concurrency
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

class NonSendable {} // expected-note {{}}

@MainActor var global = NonSendable()

@MainActor
struct MainActorIsolatedStruct {
  init() {}

  func syncMethod() {}
  func asyncMethod() {}
}

struct NonisolatedStruct {
  // Validate we can still not access global state.
  func asyncMethod() async {
    let _ = await global // expected-error {{non-Sendable type 'NonSendable' of var 'global' cannot exit main actor-isolated context}}

    let x = await MainActorIsolatedStruct()
    await x.syncMethod()
    await x.asyncMethod()
  }
}
