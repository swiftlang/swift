// RUN: %target-typecheck-verify-swift -swift-version 6 -enable-upcoming-feature NonisolatedNonsendingByDefault %s

// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

func testCasts() {
  let defaultedType = (() async -> ()).self
  _ = defaultedType as (@concurrent () async -> ()).Type
  // expected-error@-1 {{cannot convert value of type '(nonisolated(nonsending) () async -> ()).Type' to type '(() async -> ()).Type' in coercion}}
  _ = defaultedType as (nonisolated(nonsending) () async -> ()).Type // Ok
}

func test(@_inheritActorContext fn: @Sendable () async -> Void) {
  let _: Int = fn
  // expected-error@-1 {{cannot convert value of type '@Sendable () async -> Void' to specified type 'Int'}}
}
