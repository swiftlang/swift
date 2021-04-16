// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// https://bugs.swift.org/browse/SR-14466
// UNSUPPORTED: OS=windows-msvc

import _Concurrency
import StdlibUnittest


struct SomeError: Error, Equatable {
  var value = Int.random(in: 0..<100)
}

let sleepInterval: UInt64 = 125_000_000
var tests = TestSuite("YieldingContinuation")

func forceBeingAsync() async -> Void { }

@main struct Main {

static func main() async {
    if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
      tests.test("yield with no awaiting next") {
        let task = detach {
          let continuation = YieldingContinuation(yielding: String.self)
          expectFalse(continuation.yield("hello"))
          await forceBeingAsync()
        }
        await task.get()
      }

      tests.test("yield throwing with no awaiting next") {
        let task = detach {
          let continuation = YieldingContinuation(yielding: String.self, throwing: Error.self)
          expectFalse(continuation.yield(throwing: SomeError()))
          await forceBeingAsync()
        }
        await task.get()
      }

      tests.test("yield success result no awaiting next") {
        let task = detach {
          let continuation = YieldingContinuation(yielding: String.self)
          expectFalse(continuation.yield(with: .success("hello")))
          await forceBeingAsync()
        }
        await task.get()
      }

      tests.test("yield failure result no awaiting next") {
        let task = detach {
          let continuation = YieldingContinuation(yielding: String.self, throwing: Error.self)
          expectFalse(continuation.yield(with: .failure(SomeError())))
          await forceBeingAsync()
        }
        await task.get()
      }

      tests.test("yield with awaiting next") {
        let task = detach {
          let continuation = YieldingContinuation(yielding: String.self)
          let t = detach {
            let value = await continuation.next()
            expectEqual(value, "hello")
          }
          await Task.sleep(sleepInterval)
          expectTrue(continuation.yield("hello"))
          await t.get()
        }
        await task.get()
      }

      tests.test("yield result with awaiting next") {
        let task = detach {
          let continuation = YieldingContinuation(yielding: String.self)
          let t = detach {
            let value = await continuation.next()
            expectEqual(value, "hello")
          }
          await Task.sleep(sleepInterval)
          expectTrue(continuation.yield(with: .success("hello")))
          await t.get()
        }
        await task.get()
      }

      tests.test("yield throwing with awaiting next") {
        let task = detach {
          let continuation = YieldingContinuation(yielding: String.self, throwing: Error.self)
          let failure = SomeError()
          let t = detach {
            do {
              let value = try await continuation.next()
              expectUnreachable()
            } catch {
              if let error = error as? SomeError {
                expectEqual(error, failure)
              } else {
                expectUnreachable()
              }
            }
          }
          await Task.sleep(sleepInterval)
          expectTrue(continuation.yield(throwing: failure))
          await t.get()
        }
        await task.get()
      }

      tests.test("yield failure with awaiting next") {
        let task = detach {
          let continuation = YieldingContinuation(yielding: String.self, throwing: Error.self)
          let failure = SomeError()
          let t = detach {
            do {
              let value = try await continuation.next()
              expectUnreachable()
            } catch {
              if let error = error as? SomeError {
                expectEqual(error, failure)
              } else {
                expectUnreachable()
              }
            }
          }
          await Task.sleep(sleepInterval)
          expectTrue(continuation.yield(with: .failure(failure)))
          await t.get()
        }
        await task.get()
      }

      tests.test("yield multiple times with awaiting next") {
        let task = detach {
          let continuation = YieldingContinuation(yielding: String.self)
          let t = detach {
            let value1 = await continuation.next()
            expectEqual(value1, "hello")
            let value2 = await continuation.next()
            expectEqual(value2, "world")
          }
          await Task.sleep(sleepInterval)
          expectTrue(continuation.yield("hello"))
          await Task.sleep(sleepInterval)
          expectTrue(continuation.yield("world"))
          await t.get()
        }
        await task.get()
      }

      tests.test("yield result multiple times with awaiting next") {
        let task = detach {
          let continuation = YieldingContinuation(yielding: String.self)
          let t = detach {
            let value1 = await continuation.next()
            expectEqual(value1, "hello")
            let value2 = await continuation.next()
            expectEqual(value2, "world")
          }
          await Task.sleep(sleepInterval)
          expectTrue(continuation.yield(with: .success("hello")))
          await Task.sleep(sleepInterval)
          expectTrue(continuation.yield(with: .success("world")))
          await t.get()
        }
        await task.get()
      }

      tests.test("yield throwing multiple times with awaiting next") {
        let task = detach {
          let continuation = YieldingContinuation(yielding: String.self, throwing: Error.self)
          let failure1 = SomeError()
          let failure2 = SomeError()
          let t = detach {
            do {
              let value1 = try await continuation.next()
              expectUnreachable()
            } catch {
              if let error = error as? SomeError {
                expectEqual(error, failure1)
              } else {
                expectUnreachable()
              }
            }
            do {
              let value2 = try await continuation.next()
            } catch {
              if let error = error as? SomeError {
                expectEqual(error, failure2)
              } else {
                expectUnreachable()
              }
            }
          }
          await Task.sleep(sleepInterval)
          expectTrue(continuation.yield(throwing: failure1))
          await Task.sleep(sleepInterval)
          expectTrue(continuation.yield(throwing: failure2))
          await t.get()
        }
        await task.get()
      }

      tests.test("yield failure multiple times with awaiting next") {
        let task = detach {
          let continuation = YieldingContinuation(yielding: String.self, throwing: Error.self)
          let failure1 = SomeError()
          let failure2 = SomeError()
          let t = detach {
            do {
              let value1 = try await continuation.next()
              expectUnreachable()
            } catch {
              if let error = error as? SomeError {
                expectEqual(error, failure1)
              } else {
                expectUnreachable()
              }
            }
            do {
              let value2 = try await continuation.next()
            } catch {
              if let error = error as? SomeError {
                expectEqual(error, failure2)
              } else {
                expectUnreachable()
              }
            }
          }
          await Task.sleep(sleepInterval)
          expectTrue(continuation.yield(with: .failure(failure1)))
          await Task.sleep(sleepInterval)
          expectTrue(continuation.yield(with: .failure(failure2)))
          await t.get()
        }
        await task.get()
      }

      tests.test("concurrent value consumption") {
        let task = detach {
          let continuation = YieldingContinuation(yielding: String.self)
          let t1 = detach {
            var result = await continuation.next()
            expectEqual(result, "hello")
            result = await continuation.next()
            expectEqual(result, "world")
          }

          let t2 = detach {
            var result = await continuation.next()
            expectEqual(result, "hello")
            result = await continuation.next()
            expectEqual(result, "world")
          }

          await Task.sleep(sleepInterval)
          continuation.yield("hello")
          await Task.sleep(sleepInterval)
          continuation.yield("world")
          await t1.get()
          await t2.get()
        }
        await task.get()
      }
    }
    await runAllTestsAsync()
  }
}
