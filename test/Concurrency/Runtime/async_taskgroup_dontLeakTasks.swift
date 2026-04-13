// RUN: %target-run-simple-leaks-swift( -target %target-swift-5.1-abi-triple -parse-as-library)
// RUN: %target-run-simple-leaks-swift( -target %target-swift-5.1-abi-triple -parse-as-library -swift-version 5 -strict-concurrency=complete -enable-upcoming-feature NonisolatedNonsendingByDefault)
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: OS=macosx

// UNSUPPORTED: back_deployment_runtime

// Use this class to detect if the values are retained longer than necessary
final class Something {
  let int: Int
  init(int: Int) {
    self.int = int
  }
}

func test_taskGroup_next() async {
  let tasks = 5
  _ = await withTaskGroup(of: Something.self, returning: Int.self) { group in
    for n in 0..<tasks {
      group.addTask {
        Something(int: n)
      }
    }

    var sum = 0
    for await value in group {
      // Uncomment to simulate a leak and verify the `leaks` detection actually works:
      // _ = Unmanaged.passRetained(value)
      sum += value.int
    }

    return sum
  }
}

@main struct Main {
  static func main() async {
    await test_taskGroup_next()
  }
}
