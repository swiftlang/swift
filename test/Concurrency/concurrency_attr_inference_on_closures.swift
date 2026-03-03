// RUN: %target-typecheck-verify-swift -swift-version 5
// RUN: %target-typecheck-verify-swift -swift-version 5 -strict-concurrency=complete
// RUN: %target-typecheck-verify-swift -swift-version 6

// rdar://131524246

protocol P: Sendable {
  typealias Block = @Sendable () -> Void
  var block: Block? { get }
}

extension P {
  var block: Block? { nil }
}

final class Impl: P, @unchecked Sendable {
  var block: Block?
}

func test(_ v: Impl) {
  v.block = {} // Ok, no warnings or errors
}
