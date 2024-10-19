// RUN: %target-run-simple-swift(-target %target-swift-5.1-abi-triple -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: freestanding

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@available(SwiftStdlib 5.1, *)
protocol Go: Actor {
  func go(times: Int) async -> Int
}

@available(SwiftStdlib 5.1, *)
extension Go {
  func go(times: Int) async -> Int {
    for i in 0...times {
      print("\(Self.self) @ \(i)")
      await Task.yield()
    }
    return times
  }
}

@available(SwiftStdlib 5.1, *)
actor One: Go {}
@available(SwiftStdlib 5.1, *)
actor Two: Go {}

@available(SwiftStdlib 5.1, *)
func yielding() async {
  let one = One()
  let two = Two()
  await withTaskGroup(of: Int.self) { group in
    group.addTask {
      await one.go(times: 100)
    }
    group.addTask {
      await two.go(times: 100)
    }
  }
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await yielding()
  }
}
