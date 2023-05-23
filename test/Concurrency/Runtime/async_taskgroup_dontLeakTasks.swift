// RUN: %target-run-simple-leaks-swift( -Xfrontend -disable-availability-checking -parse-as-library)

// This test uses `leaks` which is only available on apple platforms; limit it to macOS:
// REQUIRES: OS=macosx

// REQUIRES: executable_test
// REQUIRES: concurrency

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Darwin

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
      group.spawn {
        Something(int: n)
      }
    }

    var sum = 0
    for await value in group {
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
