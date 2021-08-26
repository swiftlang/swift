// RUN: %target-typecheck-verify-swift -enable-experimental-distributed -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor DA {
}

@available(SwiftStdlib 5.5, *)
distributed actor First {
  distributed func one(second: Second) async throws {
    try await second.two(first: self, second: second)
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor Second {
  distributed func two(first: First, second: Second) async {
    try! await first.one(second: self)
  }
}

// ==== ------------------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
extension First {
  @_dynamicReplacement (for :_remote_one(second:))
  nonisolated func _impl_one(second: Second) async throws {
    fatalError()
  }
}

@available(SwiftStdlib 5.5, *)
extension Second {
  @_dynamicReplacement (for :_remote_two(first:second:))
  nonisolated func _impl_two(first: First, second: Second) async throws {
    fatalError()
  }
}