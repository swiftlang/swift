// RUN: %target-swift-frontend  -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend  -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend  -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=complete
// RUN: %target-swift-frontend  -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: swift_feature_RegionBasedIsolation

protocol Iterator {
  associatedtype Failure: Error
  mutating func next() async throws -> Int?
}

extension Iterator {
  mutating func next() async throws(Failure) -> Int? {
    nil
  }
}

actor C: Iterator {
  typealias Failure = any Error
  func next() throws -> Int? { nil }
}

actor A {
  let c = C()

  init() async {
    _ = try! await self.c.next()
  }
}
