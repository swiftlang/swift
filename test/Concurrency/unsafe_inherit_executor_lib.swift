// RUN: %target-swift-frontend -emit-sil -verify -o /dev/null -disable-availability-checking %s -strict-concurrency=complete
// RUN: %target-swift-frontend -emit-sil -verify -o /dev/null -disable-availability-checking %s -strict-concurrency=complete -enable-experimental-feature RegionBasedIsolation

// REQUIRES: asserts

actor A {
  func g() { }
  func h() throws { }
  
  func f() async throws {
    await withTaskGroup(of: Int.self, returning: Void.self) { group in
      g()
    }

    try await withThrowingTaskGroup(of: Int.self, returning: Void.self) { group in
      try h()
    }
  }
}
