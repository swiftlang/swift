// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: freestanding

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

enum SomeError: Error {
  case bad
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    let condition = false

    let t1 = Task {
      return 5
    }

    let t2 = Task { () -> Int in
      if condition {
        throw SomeError.bad
      }

      return 7
    }

    let t3 = Task.detached {
      return 9
    }

    let t4 = Task.detached { () -> Int in
      if condition {
        throw SomeError.bad
      }

      return 11
    }

    let result = try! await t1.get() + t2.get() + t3.get() + t4.get()
    assert(result == 32)
  }
}
