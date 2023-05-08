// RUN: %target-run-simple-leaks-swift( -Xfrontend -disable-availability-checking -parse-as-library)

// This test uses `leaks` which is only available on apple platforms; limit it to macOS:
// REQUIRES: OS=macosx

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

// FIXME: enable discarding taskgroup on windows; rdar://104762037
// UNSUPPORTED: OS=windows-msvc

import _Concurrency

final class ClassBoom: Error {
  let id: String

  init(file: String = #fileID, line: UInt = #line) {
    self.id = "\(file):\(line)"
  }

  init(id: String) {
    self.id = id
  }
}

@main struct Main {
  static func main() async {

    // many errors
    _ = try? await withThrowingDiscardingTaskGroup() { group in
      group.addTask {
        throw ClassBoom()
      }
      group.addTask {
        throw ClassBoom()
      }
      group.addTask {
        throw ClassBoom()
      }
      group.addTask {
        throw ClassBoom()
      }
      group.addTask {
        12
      }

      return 12
    }
  }
}
