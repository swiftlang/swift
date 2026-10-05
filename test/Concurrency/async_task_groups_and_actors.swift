// RUN: %target-swift-frontend  -target %target-swift-5.9-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

// REQUIRES: concurrency
// REQUIRES: libdispatch

@MainActor
class MyActor {
  func check() async throws {
    await withTaskGroup(of: Int.self) { group in
      group.addTask {
        2
      }
      await group.waitForAll()
    }

    try await withThrowingTaskGroup(of: Int.self) { throwingGroup in
      throwingGroup.addTask {
        2
      }
      try await throwingGroup.waitForAll()
    }

    await withDiscardingTaskGroup { discardingGroup in
      discardingGroup.addTask {
        ()
      }
    }

    try await withThrowingDiscardingTaskGroup { throwingDiscardingGroup in
      throwingDiscardingGroup.addTask {
        ()
      }
    }
  }
}
