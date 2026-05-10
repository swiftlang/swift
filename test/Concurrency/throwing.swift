// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: freestanding

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import _Concurrency
import StdlibUnittest

class P<T> {
  var t: T
  init(_ v: T) {
    t = v
  }
}

class A {}
class B {}
class C {}

enum E : Error {
    case err
}

protocol MP { }

class M : MP {

  @available(SwiftStdlib 5.1, *)
  func throwWithIndirectResult<T>(_ a: P<T>) async throws -> T {
    throw E.err
  }
}

extension MP {
  @available(SwiftStdlib 5.1, *)
  func l<A, B, C, D, E2, F> (_ a : P<A>, _ b: P<B>, _ c: P<C>, _ d : P<D>, _ e: P<E2>, _ f: P<F>) async throws -> (A, B, C, D, E2, F) {
    throw E.err
  }
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("Async Throw")

    if #available(SwiftStdlib 5.1, *) {
      tests.test("throwing of naturally direct but indirect reabstraction") {
        let task2 = detach {
          let m = M()
          await verifyCancelled {
            try await m.l(P(A()), P(B()), P(C()), P(A()), P(B()), P(C()))
          }
          func verifyCancelled<T>(execute operation: () async throws -> T) async {
            do {
              let _ = try await operation()
              assertionFailure("operation() threw")
            }
            catch _ as E {
              // This is what we expect to happen
            }
            catch {
             assertionFailure("unknown error thrown")
            }
          }
        }
        _ = await task2.get()
      }
      tests.test("throwing with indirect result") {
        let task2 = detach {
          let m = M()
          do {
            let _ = try await m.throwWithIndirectResult(P(A()))
            assertionFailure("operation() threw")
          }
          catch _ as E {
            // This is what we expect to happen
          }
          catch {
           assertionFailure("unknown error thrown")
          }
        }
        _ = await task2.get()
      }
    }
    await runAllTestsAsync()
  }
}
