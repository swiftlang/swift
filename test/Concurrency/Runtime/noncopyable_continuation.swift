// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library) 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import _Concurrency
import StdlibUnittest

struct TestError: Error {}

struct UniqueResource: ~Copyable {
  let value: Int
  init(_ value: Int) {
    self.value = value
  }

  deinit {
    print("UniqueResource(\(value)).deinit")
  }
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("Continuation: ~Copyable")

    if #available(SwiftStdlib 6.4, *) {
      #if !os(WASI)
      tests.test("trap on dropped continuation") {
        expectCrashLater()

        let task = Task.detached {
          let _: Void = await withContinuation { c in
            _ = consume c // don't do this
          }
        }
        await task.value
      }

      tests.test("trap on dropped throwing continuation") {
        expectCrashLater()

        do {
          let _: Void = try await withContinuation(of: Void.self, throwing: (any Error).self) { c in
            _ = consume c // bad! should have resumed
          }
        } catch {}
      }
      #endif

      tests.test("resume returning value") {
        let value: Int = await withContinuation { c in
          c.resume(returning: 42)
        }
        expectEqual(42, value)
      }

      tests.test("resume void") {
        await withContinuation { c in
          c.resume()
        }
      }

      tests.test("throwing continuation resume returning") {
        do {
          let value: Int = try await withContinuation(of: Int.self, throwing: (any Error).self) { c in
            c.resume(returning: 42)
          }
          expectEqual(42, value)
        } catch {
          expectUnreachable()
        }
      }

      tests.test("throwing continuation resume throwing") {
        do {
          let _: Int = try await withContinuation(of: Int.self, throwing: (any Error).self) { c in
            c.resume(throwing: TestError())
          }
          expectUnreachable()
        } catch {
          // Expected
        }
      }

      tests.test("resume with result success") {
        let value: Int = try! await withContinuation(of: Int.self, throwing: (any Error).self) { c in
          c.resume(with: .success(17))
        }
        expectEqual(17, value)
      }

      tests.test("resume with result failure") {
        do {
          let _: Int = try await withContinuation(of: Int.self, throwing: (any Error).self) { c in
            c.resume(with: .failure(TestError()))
          }
          expectUnreachable()
        } catch {
          // Expected
        }
      }

      // MARK: Conversion to CheckedContinuation

      tests.test("convert to CheckedContinuation and resume returning") {
        let value: Int = await withContinuation { c in
          let checked = CheckedContinuation(c)
          checked.resume(returning: 42)
        }
        expectEqual(42, value)
      }

      tests.test("convert throwing to CheckedContinuation and resume returning") {
        do {
          let value: Int = try await withContinuation(of: Int.self, throwing: (any Error).self) { c in
            let checked = CheckedContinuation(c)
            checked.resume(returning: 99)
          }
          expectEqual(99, value)
        } catch {
          expectUnreachable()
        }
      }

      tests.test("convert throwing to CheckedContinuation and resume throwing") {
        do {
          let _: Int = try await withContinuation(of: Int.self, throwing: (any Error).self) { c in
            let checked = CheckedContinuation(c)
            checked.resume(throwing: TestError())
          }
          expectUnreachable()
        } catch {
          // Expected
        }
      }

      tests.test("convert typed-throws to CheckedContinuation and resume throwing") {
        struct MyCoolError: Error, Equatable {}
        do {
          try await withContinuation(of: Void.self, throwing: MyCoolError.self) { c in
            let checked = CheckedContinuation(c)
            checked.resume(throwing: MyCoolError())
          }
          expectUnreachable()
        } catch let error as MyCoolError {
          expectEqual(MyCoolError(), error)
        } catch {
          expectUnreachable()
        }
      }

      // MARK: Conversion to UnsafeContinuation

      tests.test("convert to UnsafeContinuation and resume returning") {
        let value: Int = await withContinuation { c in
          let uc = UnsafeContinuation(c)
          uc.resume(returning: 7)
        }
        expectEqual(7, value)
      }

      tests.test("convert throwing to UnsafeContinuation and resume returning") {
        do {
          let value: Int = try await withContinuation(of: Int.self, throwing: (any Error).self) { c in
            let uc = UnsafeContinuation(c)
            uc.resume(returning: 55)
          }
          expectEqual(55, value)
        } catch {
          expectUnreachable()
        }
      }

      tests.test("convert throwing to UnsafeContinuation and resume throwing") {
        do {
          let _: Int = try await withContinuation(of: Int.self, throwing: (any Error).self) { c in
            let uc = UnsafeContinuation(c)
            uc.resume(throwing: TestError())
          }
          expectUnreachable()
        } catch {
          // Expected
        }
      }

      tests.test("noncopyable resume returning value") {
        let resource: UniqueResource = await withContinuation { c in
          c.resume(returning: UniqueResource(99))
          print("non-throwing resume done")
          // CHECK: non-throwing resume done
        }
        print("non-throwing resumed with: \(resource.value)")
        // CHECK: non-throwing resumed with: 99
        expectEqual(99, resource.value)
        _ = consume resource
        // CHECK: UniqueResource(99).deinit
      }

      tests.test("noncopyable throwing continuation resume returning") {
        do {
          let resource: UniqueResource = try await withContinuation(of: UniqueResource.self, throwing: (any Error).self) { c in
            c.resume(returning: UniqueResource(77))
            print("throwing resume done")
            // CHECK: throwing resume done
          }
          print("throwing resumed with: \(resource.value)")
          // CHECK: throwing resumed with: 77
          expectEqual(77, resource.value)
          _ = consume resource
          // CHECK: UniqueResource(77).deinit
        } catch {
          expectUnreachable()
        }
      }

      tests.test("noncopyable throwing continuation resume throwing") {
        do {
          let _: UniqueResource = try await withContinuation(of: UniqueResource.self, throwing: (any Error).self) { c in
            c.resume(throwing: TestError())
          }
          expectUnreachable()
        } catch {
          // Expected
        }
      }

      tests.test("noncopyable resume with result success") {
        let resource: UniqueResource = try! await withContinuation(of: UniqueResource.self, throwing: (any Error).self) { c in
          c.resume(with: .success(UniqueResource(55)))
          print("result resume done")
          // CHECK: result resume done
        }
        print("result resumed with: \(resource.value)")
        // CHECK: result resumed with: 55
        expectEqual(55, resource.value)
        _ = consume resource
        // CHECK: UniqueResource(55).deinit
      }

      tests.test("noncopyable resume with result failure") {
        do {
          let _: UniqueResource = try await withContinuation(of: UniqueResource.self, throwing: (any Error).self) { c in
            c.resume(with: .failure(TestError()))
          }
          expectUnreachable()
        } catch {
          // Expected
        }
      }
    }

    await runAllTestsAsync()
  }
}
