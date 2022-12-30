// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking)

// REQUIRES: concurrency
// REQUIRES: executable_test

// rdar://102155748
// UNSUPPORTED: back_deployment_runtime

func runIn10ms(_ closure: @escaping @Sendable () -> Void) {
  Task {
    try! await Task.sleep(nanoseconds: 10_000_000)
    closure()
  }
}

final class Weak: Sendable {
  let property = "Self exists"

  func test() {
    runIn10ms { [weak self] in
      if let self {
        // Use implicit self -- this should not result in a strong capture
        _ = property
        fatalError("Self was unexpectedly captured strongly")
      } else {
        print("Self was captured weakly")
      }
    }
  }
}

Weak().test()
try await Task.sleep(nanoseconds: 20_000_000)
