// RUN: %target-typecheck-verify-swift -enable-experimental-distributed -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

distributed actor DA {
}

distributed actor First {
  distributed func one(second: Second) async throws {
    try await second.two(first: self, second: second)
  }
}

distributed actor Second {
  distributed func two(first: First, second: Second) async {
    try! await first.one(second: self)
  }
}

// ==== ------------------------------------------------------------------------

extension First {
  @_dynamicReplacement (for :_remote_one(second:))
  nonisolated func _impl_one(second: Second) async throws {
    fatalError()
  }
}

extension Second {
  @_dynamicReplacement (for :_remote_two(first:second:))
  nonisolated func _impl_two(first: First, second: Second) async throws {
    fatalError()
  }
}