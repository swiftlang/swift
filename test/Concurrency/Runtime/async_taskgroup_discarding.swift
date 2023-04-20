// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library)

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

struct IgnoredBoom: Error {}

@discardableResult
func echo(_ i: Int) -> Int { i }
@discardableResult
func boom(file: String = #fileID, line: UInt = #line) throws -> Int { throw Boom(file: file, line: line) }

func shouldEqual<T: Equatable>(_ lhs: T, _ rhs: T) {
  precondition(lhs == rhs, "'\(lhs)' did not equal '\(rhs)'")
}
func shouldStartWith(_ lhs: Any, _ rhs: Any) {
  let l = "\(lhs)"
  let r = "\(rhs)"
  precondition(l.prefix("\(r)".count) == r, "'\(l)' did not start with '\(r)'")
}

// NOTE: Not as StdlibUnittest/TestSuite since these types of tests are unreasonably slow to load/debug.

@main struct Main {
  static func main() async {
    for i in 0...1_000 {
      do {
        let got = try await withThrowingDiscardingTaskGroup() { group in
          group.addTask {
            echo(1)
          }
          group.addTask {
            try boom()
          }

          return 12
        }
        fatalError("(iteration:\(i)) expected error to be re-thrown, got: \(got)")
      } catch {
        shouldStartWith(error, "Boom(")
      }
    }
  }
}
