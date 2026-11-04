// RUN: %target-run-simple-swift(-target %target-swift-6.0-abi-triple -parse-as-library) | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// https://github.com/swiftlang/swift/issues/87591
//
// A `rethrows` AsyncSequence operation called from a context that throws the
// sequence's `Failure` must deliver the error with its typed value intact.

struct MyErr: Error {
  let code: Int
}

// Yields `values`, then throws `error` if there is one.
struct Seq<E: Error>: AsyncSequence {
  typealias Element = Int
  typealias Failure = E

  let values: [Int]
  let error: E?

  struct AsyncIterator: AsyncIteratorProtocol {
    var values: [Int]
    let error: E?

    mutating func next(isolation actor: isolated (any Actor)?) async throws(E) -> Int? {
      if !values.isEmpty { return values.removeFirst() }
      if let error { throw error }
      return nil
    }

    mutating func next() async throws -> Int? { try await next(isolation: nil) }
  }

  func makeAsyncIterator() -> AsyncIterator {
    AsyncIterator(values: values, error: error)
  }
}

func concrete(_ s: Seq<MyErr>) async throws(MyErr) -> Int? {
  try await s.first(where: { $0 > 1 })
}

func generic<S: AsyncSequence>(_ s: S) async throws(S.Failure) -> S.Element? {
  try await s.first(where: { _ in true })
}

extension AsyncSequence {
  func firstOrNil() async -> Element where Element: ExpressibleByNilLiteral {
    do {
      return try await first(where: { _ in true }) ?? nil
    } catch {
      return nil
    }
  }
}

@main struct Main {
  static func main() async {
    // CHECK: concrete value: Optional(2)
    do {
      print("concrete value:", try await concrete(Seq(values: [1, 2, 3], error: nil)) as Any)
    } catch {
      print("unexpected error", error.code)
    }

    // CHECK: concrete caught 42
    do {
      _ = try await concrete(Seq(values: [0], error: MyErr(code: 42)))
    } catch {
      print("concrete caught", error.code)
    }

    // CHECK: generic caught 7
    do {
      _ = try await generic(Seq(values: [], error: MyErr(code: 7)))
    } catch {
      print("generic caught", error.code)
    }

    // CHECK: never failing: Optional(5)
    print("never failing:", await generic(Seq<Never>(values: [5], error: nil)) as Any)

    // CHECK: firstOrNil: Optional(9)
    let opt = AsyncStream<Int?> { c in c.yield(9); c.finish() }
    print("firstOrNil:", await opt.firstOrNil() as Any)
  }
}
