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

struct Boom: Error {
  let id: String

  init(file: String = #fileID, line: UInt = #line) {
    self.id = "\(file):\(line)"
  }

  init(id: String) {
    self.id = id
  }
}

final class BoomClass: Error {
  let id: String

  init(file: String = #fileID, line: UInt = #line) {
    self.id = "\(file):\(line)"
  }

  init(id: String) {
    self.id = id
  }
}

final class SomeClass: @unchecked Sendable {
//struct SomeClass: @unchecked Sendable {
  let number: Int
  init() {
    self.number = 0
  }
  init(number: Int) {
    self.number = number
  }
}

// NOTE: Not as StdlibUnittest/TestSuite since these types of tests are unreasonably slow to load/debug.

@main struct Main {
  static func main() async {
    _ = try? await withThrowingDiscardingTaskGroup() { group in
      group.addTask {
        throw Boom()
      }
      group.addTask {
        SomeClass() // will be discarded
      }

      return 12
    }

    // many ok
    _ = try? await withThrowingDiscardingTaskGroup() { group in
      for i in 0..<10 {
        group.addTask {
          SomeClass(number: i) // will be discarded
        }
      }

      return 12
    }

    // many throws
    do {
      let value = try await withThrowingDiscardingTaskGroup() { group in
        for i in 0..<10 {
          group.addTask {
            throw BoomClass() // will be rethrown
          }
        }

        12 // must be ignored
      }
      preconditionFailure("Should throw")
    } catch {
      precondition("\(error)" == "main.BoomClass", "error was: \(error)")
    }

    // many errors, many values
    _ = try? await withThrowingDiscardingTaskGroup() { group in
      group.addTask {
        SomeClass() // will be discarded
      }
      group.addTask {
        SomeClass() // will be discarded
      }
      group.addTask {
        SomeClass() // will be discarded
      }
      group.addTask {
        throw Boom()
      }
      group.addTask {
        throw Boom()
      }
      group.addTask {
        throw Boom()
      }
      group.addTask {
        throw Boom()
      }

      return 12
    }
  }
}
