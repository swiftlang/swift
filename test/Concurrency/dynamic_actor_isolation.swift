// RUN: %target-typecheck-verify-swift -swift-version 5 -strict-concurrency=complete -enable-upcoming-feature DynamicActorIsolation -verify-additional-prefix swift6-
// RUN: %target-typecheck-verify-swift -swift-version 6 -verify-additional-prefix swift6-

// REQUIRES: swift_feature_DynamicActorIsolation

// Tests related to DynamicActorIsolation feature

// rdar://142562250 - error: call can throw, but it is not marked with ‘try’ and the error is not handled
@MainActor
struct TestNoErrorsAboutThrows {
  struct Column {
    @MainActor
    init?(_ column: Int) {}
  }

  func test(columns: [Int]) {
    // MainActor isolation erasure shouldn't interfere with effects checking
    _ = columns.compactMap(Column.init) // Ok
  }
}
