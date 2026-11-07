// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name main -target %target-swift-5.7-abi-triple -j2 -parse-as-library %s %S/../Inputs/SilentRoundtripActorSystem.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -enable-sil-opaque-values -module-name main -target %target-swift-5.7-abi-triple -j2 -parse-as-library %s %S/../Inputs/SilentRoundtripActorSystem.swift -o %t/a-opaque-values.out
// RUN: %target-codesign %t/a-opaque-values.out
// RUN: %target-run %t/a-opaque-values.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: OS=windows-msvc

// The runtime resolves a target's signature on its first call and reuses it
// afterwards; every later call must still decode the arguments as the right
// types and return the right value, for every kind of target, and so must
// first calls made at the same time on several threads.

import Distributed

typealias DefaultDistributedActorSystem = SilentRoundtripActorSystem

struct WorkerError: Error, Codable {
  let code: Int
}

/// Suspends every task that arrives until `count` of them have, then resumes
/// them all at once. The tasks wait on continuations, so more of them than the
/// cooperative pool has threads cannot deadlock it.
actor StartLatch {
  private let count: Int
  private var waiting: [CheckedContinuation<Void, Never>] = []

  init(count: Int) {
    self.count = count
  }

  func arrive() async {
    await withCheckedContinuation { continuation in
      waiting.append(continuation)
      if waiting.count == count {
        for waiter in waiting { waiter.resume() }
        waiting.removeAll()
      }
    }
  }
}

distributed actor Worker {
  distributed func nothing() {}
  distributed func noArguments() -> Int { 42 }
  distributed func twoInts(_ a: Int, _ b: Int) -> Int { a &+ b }
  distributed func stringAndArray(_ s: String, _ values: [Int]) -> String {
    "\(s):\(values.count)"
  }
  distributed func generic<T: Codable & Sendable>(_ value: T) -> T { value }
  distributed func failing(_ code: Int) throws -> Int {
    throw WorkerError(code: code)
  }
  // Called from the concurrent phase only, so that their first calls can race.
  distributed func racing(_ a: Int, _ s: String) -> String { "\(s)\(a)" }
  distributed func racingGeneric<T: Codable & Sendable>(_ value: T) -> T { value }
}

@main struct Main {
  static func main() async throws {
    let system = DefaultDistributedActorSystem()
    let worker = Worker(actorSystem: system)
    let ref = try Worker.resolve(id: worker.id, using: system)

    var mismatches = 0
    for i in 0..<1_000 {
      try await ref.nothing()
      if try await ref.noArguments() != 42 { mismatches += 1 }
      if try await ref.twoInts(i, 1) != i + 1 { mismatches += 1 }
      if try await ref.stringAndArray("s\(i)", Array(repeating: 0, count: i % 5)) != "s\(i):\(i % 5)" {
        mismatches += 1
      }
      if try await ref.generic(i) != i { mismatches += 1 }
      if try await ref.generic("g\(i)") != "g\(i)" { mismatches += 1 }
      do {
        _ = try await ref.failing(i)
        mismatches += 1
      } catch let error as WorkerError where error.code == i {
        // expected
      } catch {
        mismatches += 1
      }
    }

    // First calls to targets not called before, from 64 tasks released by a
    // latch once all of them are waiting, so that the first calls to each target
    // set off together on as many threads as the pool has. That makes it likely,
    // though not certain, that several of them miss the cache at once and
    // resolve the same signature, of which the runtime must keep one; every
    // caller must get the right result either way.
    let taskCount = 64
    let latch = StartLatch(count: taskCount)
    mismatches += try await withThrowingTaskGroup(of: Int.self) { group in
      for i in 0..<taskCount {
        group.addTask {
          await latch.arrive()
          var raceMismatches = 0
          if try await ref.racing(i, "r") != "r\(i)" { raceMismatches += 1 }
          if try await ref.racingGeneric(i) != i { raceMismatches += 1 }
          return raceMismatches
        }
      }
      return try await group.reduce(0, +)
    }

    // CHECK: mismatches: 0
    print("mismatches: \(mismatches)")
  }
}
