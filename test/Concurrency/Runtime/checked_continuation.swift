// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library)
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -swift-version 5 -strict-concurrency=complete -enable-upcoming-feature NonisolatedNonsendingByDefault)
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: reflection
// UNSUPPORTED: back_deployment_runtime

import _Concurrency
import StdlibUnittest

struct TestError: Error {}
enum MyError: Error, Equatable {
  case fail
  case other
}

@main struct Main {
  static func main() async {
    var tests = TestSuite("CheckedContinuation")

    if #available(SwiftStdlib 5.1, *) {
      #if !os(WASI)
      tests.test("trap on double resume non-throwing continuation") {
        expectCrashLater()

        let task = detach {
          let _: Int = await withCheckedContinuation { c in
            c.resume(returning: 17)
            c.resume(returning: 38)
          }
        }
        await task.get()
      }

      tests.test("trap on double resume throwing continuation") {
        expectCrashLater()

        let task = detach {
          do {
            let _: Int = try await withCheckedThrowingContinuation { c in
              c.resume(returning: 17)
              c.resume(throwing: TestError())
            }
          } catch {
          }
        }
        await task.get()
      }
      #endif

      tests.test("test withCheckedThrowingContinuation") {
        let task2 = detach {
          do {
            let x: Int = try await withCheckedThrowingContinuation { c in
              c.resume(returning: 17)
            }
            expectEqual(17, x)
          } catch {
          }
        }

        let task = detach {
          do {
            let x: Int = try await withCheckedThrowingContinuation { c in
              c.resume(returning: 17)
            }
            expectEqual(17, x)
          } catch {
          }
        }
        await task.get()
        await task2.get()
      }

      // ==== Typed throws: withCheckedContinuation ====

      tests.test("withCheckedContinuation: resume returning with Never error") {
        let result: Int = await withCheckedContinuation { c in
          expectEqual(type(of: c), CheckedContinuation<Int, Never>.self)
          c.resume(returning: 42)
        }
        expectEqual(42, result)
      }

      tests.test("withCheckedContinuation: resume returning with typed error") {
        do {
          let result: Int = try await withCheckedContinuation { (c: CheckedContinuation<Int, MyError>) in
            c.resume(returning: 99)
          }
          expectEqual(99, result)
        } catch {
          expectUnreachable("should not throw")
        }
      }

      tests.test("withCheckedContinuation: resume throwing with typed error") {
        do throws(MyError) {
          let _: Int = try await withCheckedContinuation { (c: CheckedContinuation<Int, MyError>) in
            c.resume(throwing: .fail)
          }
          expectUnreachable("should have thrown")
        } catch {
          expectEqual(.fail, error)
        }
      }

      tests.test("withCheckedContinuation: resume returning with any Error") {
        do {
          let result: Int = try await withCheckedContinuation { (c: CheckedContinuation<Int, any Error>) in
            c.resume(returning: 17)
          }
          expectEqual(17, result)
        } catch {
          expectUnreachable("should not throw")
        }
      }

      tests.test("withCheckedContinuation: resume throwing with any Error") {
        do {
          let _: Int = try await withCheckedContinuation { (c: CheckedContinuation<Int, any Error>) in
            c.resume(throwing: MyError.other)
          }
          expectUnreachable("should have thrown")
        } catch {
          expectTrue(error is MyError)
        }
      }

      tests.test("withCheckedContinuation: resume with Result success (typed)") {
        do {
          let result: Int = try await withCheckedContinuation { (c: CheckedContinuation<Int, MyError>) in
            c.resume(with: .success(55))
          }
          expectEqual(55, result)
        } catch {
          expectUnreachable("should not throw")
        }
      }

      tests.test("withCheckedContinuation: resume with Result failure (typed)") {
        do throws(MyError) {
          let _: Int = try await withCheckedContinuation { c in
            c.resume(with: .failure(MyError.fail))
          }
          expectUnreachable("should have thrown")
        } catch {
          expectEqual(.fail, error)
        }
      }

      tests.test("withCheckedContinuation: Void return with Never error") {
        await withCheckedContinuation { c in
          expectEqualType(type(of: c), CheckedContinuation<Void, Never>.self)
          c.resume()
        }
      }

      tests.test("withCheckedContinuation: Void return with typed error") {
        do {
          try await withCheckedContinuation { (c: CheckedContinuation<Void, MyError>) in
            c.resume()
          }
        } catch {
          expectUnreachable("should not throw")
        }
      }

      tests.test("withCheckedContinuation: async resume from detached task (typed)") {
        do {
          let result: String = try await withCheckedContinuation { (c: CheckedContinuation<String, MyError>) in
            Task.detached {
              c.resume(returning: "hello")
            }
          }
          expectEqual("hello", result)
        } catch {
          expectUnreachable("should not throw")
        }
      }

      tests.test("withCheckedContinuation: async throw from detached task (typed)") {
        do throws(MyError) {
          let _: String = try await withCheckedContinuation { (c: CheckedContinuation<String, MyError>) in
            Task.detached {
              c.resume(throwing: .other)
            }
          }
          expectUnreachable("should have thrown")
        } catch {
          expectEqual(.other, error)
        }
      }

      // ==== Typed throws: withUnsafeContinuation ====

      tests.test("withUnsafeContinuation: resume returning with Never error") {
        let result: Int = await withUnsafeContinuation { c in
          expectEqualType(type(of: c), UnsafeContinuation<Int, Never>.self)
          c.resume(returning: 42)
        }
        expectEqual(42, result)
      }

      tests.test("withUnsafeContinuation: resume returning with typed error") {
        do {
          let result: Int = try await withUnsafeContinuation { (c: UnsafeContinuation<Int, MyError>) in
            c.resume(returning: 99)
          }
          expectEqual(99, result)
        } catch {
          expectUnreachable("should not throw")
        }
      }

      tests.test("withUnsafeContinuation: resume throwing with typed error") {
        do throws(MyError) {
          let _: Int = try await withUnsafeContinuation { (c: UnsafeContinuation<Int, MyError>) in
            c.resume(throwing: .fail)
          }
          expectUnreachable("should have thrown")
        } catch {
          expectEqual(.fail, error)
        }
      }

      tests.test("withUnsafeContinuation: resume returning with any Error") {
        do {
          let result: Int = try await withUnsafeContinuation { c in
            expectEqualType(type(of: c), UnsafeContinuation<Int, any Error>.self)
            c.resume(returning: 17)
          }
          expectEqual(17, result)
        } catch {
          expectUnreachable("should not throw")
        }
      }

      tests.test("withUnsafeContinuation: resume throwing with any Error") {
        do {
          let _: Int = try await withUnsafeContinuation { c in
            // TODO: why isn't this inferred to throw MyError?
            expectEqualType(type(of: c), UnsafeContinuation<Int, any Error>.self)
            c.resume(throwing: MyError.other)
          }
          expectUnreachable("should have thrown")
        } catch {
          expectTrue(error is MyError)
        }
      }

      tests.test("withUnsafeContinuation: resume with Result success (typed)") {
        // TODO: why are these throws() annotations needed?
        do throws(MyError) {
          let result: Int = try await withUnsafeContinuation { (c: UnsafeContinuation<Int, MyError>) in
            c.resume(with: .success(55))
          }
          expectEqual(55, result)
        } catch {
          expectUnreachable("should not throw")
        }
      }

      tests.test("withUnsafeContinuation: resume with Result failure (typed)") {
        do throws(MyError) {
          let _: Int = try await withUnsafeContinuation { c in
            c.resume(with: .failure(MyError.fail))
          }
          expectUnreachable("should have thrown")
        } catch {
          expectEqual(.fail, error)
        }
      }

      tests.test("withUnsafeContinuation: Void return with Never error") {
        await withUnsafeContinuation { c in
          expectEqualType(type(of: c), UnsafeContinuation<Void, Never>.self)
          c.resume()
        }
      }

      tests.test("withUnsafeContinuation: Void return with typed error") {
        do {
          try await withUnsafeContinuation { (c: UnsafeContinuation<Void, MyError>) in
            c.resume()
          }
        } catch {
          expectUnreachable("should not throw")
        }
      }

      tests.test("withUnsafeContinuation: async resume from detached task (typed)") {
        do {
          let result: String = try await withUnsafeContinuation { (c: UnsafeContinuation<String, MyError>) in
            Task.detached {
              c.resume(returning: "world")
            }
          }
          expectEqual("world", result)
        } catch {
          expectUnreachable("should not throw")
        }
      }
    }

    await runAllTestsAsync()
  }
}
