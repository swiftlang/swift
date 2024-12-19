// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: swift_feature_RegionBasedIsolation

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
