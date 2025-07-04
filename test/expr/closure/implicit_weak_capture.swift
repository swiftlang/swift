// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking -swift-version 6)
// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking -swift-version 6 -enable-upcoming-feature ImmutableWeakCaptures)

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: swift_feature_ImmutableWeakCaptures

// rdar://102155748
// UNSUPPORTED: back_deployment_runtime

func runIn10ms(_ closure: @escaping @Sendable () -> Void) {
  Task {
    try! await Task.sleep(nanoseconds: 10_000_000)
    closure()
  }
}

let checkInterval = 10_000_000
let checkIters = 1000

final class Weak: Sendable {
  let property = "Self exists"

  func test() async -> (Task<Void, Never>, Task<Void, Never>) {
    let t1 = Task { [weak self] in
      for _ in 0..<checkIters {
        if let self {
          // Use implicit self -- this should not result in a strong capture
          _ = property
        } else {
          print("Self was captured weakly (1)")
          return
        }
        try! await Task.sleep(nanoseconds: 10_000_000)
      }
      fatalError("Self was unexpectedly captured strongly")
    }

    let t2 = Task { [weak self] in
      for _ in 0..<checkIters {
        guard let self else {
          print("Self was captured weakly (2)")
          return
        }

        // Use implicit self -- this should not result in a strong capture
        _ = property

        runIn10ms { [self] in
          // Use implicit self -- this should not result in a strong capture
          _ = property
        }
        try! await Task.sleep(nanoseconds: 10_000_000)
      }
      fatalError("Self was unexpectedly captured strongly")
    }

    return (t1, t2)
  }
}

let (t1, t2) = await Weak().test()
await t1.value
await t2.value
