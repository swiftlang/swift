// RUN: %target-run-simple-leaks-swift( -Xfrontend -disable-availability-checking -parse-as-library)

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

// FIXME: enable discarding taskgroup on windows; rdar://104762037
// UNSUPPORTED: OS=windows-msvc

import _Concurrency
import Darwin

final class FIRST_PAYLOAD {}
final class SECOND_PAYLOAD {}

final class FIRST_ERROR: Error {
  let first: FIRST_PAYLOAD

  init(file: String = #fileID, line: UInt = #line) {
    first = .init()
  }
  deinit {
    fputs("\(Self.self) deinit\n", stderr)
  }
}

final class SECOND_ERROR: Error {
  let second: SECOND_PAYLOAD

  init(file: String = #fileID, line: UInt = #line) {
    second = .init()
  }

  deinit {
    fputs("\(Self.self) deinit\n", stderr)
  }
}

func shouldStartWith(_ lhs: Any, _ rhs: Any) {
  let l = "\(lhs)"
  let r = "\(rhs)"
  precondition(l.prefix("\(r)".count) == r, "'\(l)' did not start with '\(r)'")
}

// NOTE: Not as StdlibUnittest/TestSuite since these types of tests are unreasonably slow to load/debug.

@main struct Main {
  static func main() async {
    do {
      let got = try await withThrowingDiscardingTaskGroup() { group in
        group.addTask {
          1
        }
        group.addTask {
          throw FIRST_ERROR()
        }

//        try? await Task.sleep(for: .seconds(1))

        group.addTask {
          throw SECOND_ERROR()
        }

        return 12
      }
      fatalError("expected error to be re-thrown, got: \(got)")
    } catch {
      // shouldStartWith(error, "main.Boom")
    }
  }
}
