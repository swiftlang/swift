// RUN: %target-run-simple-swift(-target %target-swift-6.0-abi-triple -parse-as-library) | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@available(SwiftStdlib 6.0, *)
struct TestSequence<Failure: Error>: AsyncSequence, AsyncIteratorProtocol, Sendable {
  let failure: Failure?
  var emitted = false

  func makeAsyncIterator() -> Self {
    let iterator = self
    return iterator
  }

  mutating func next() async throws(Failure) -> Int? {
    if let failure {
      throw failure
    }

    guard !emitted else { return nil }
    emitted = true
    return 42
  }
}

@available(SwiftStdlib 6.0, *)
@main struct Main {
  enum IterationError: Error {
    case failed
  }

  enum TransformError: Error {
    case failed
  }

  static func reduce<S: AsyncSequence & Sendable>(_ sequence: S) async throws(S.Failure) -> Int where S.Element == Int {
    async let value = sequence.reduce(0) { $0 + $1 }
    let result = try await value
    return result
  }

  static func reduceConcrete(_ sequence: TestSequence<IterationError>) async throws -> Int {
    async let value = sequence.reduce(0, +)
    let result = try await value
    return result
  }

  static func reduceGenericUntyped<S: AsyncSequence & Sendable>(_ sequence: S) async throws -> Int where S.Element == Int {
    async let value = sequence.reduce(0, +)
    let result = try await value
    return result
  }

  static func reduceWithThrowingTransform(_ sequence: TestSequence<IterationError>) async throws -> Int {
    async let value = sequence.reduce(0) { _, _ in
      throw TransformError.failed
    }
    let result = try await value
    return result
  }

  static func fail() throws(IterationError) -> Int {
    defer { print("discarded child completed") }
    throw IterationError.failed
  }

  static func discardFailure() async {
    async let _ = try fail()
  }

  static func main() async {
    let success = await reduce(TestSequence<Never>(failure: nil))
    print("sum: \(success)") // CHECK: sum: 42

    do {
      _ = try await reduce(TestSequence(failure: IterationError.failed))
    } catch {
      let typedError: IterationError = error
      print("iteration: \(typedError)") // CHECK-NEXT: iteration: failed
    }

    do {
      let sum = try await reduceConcrete(TestSequence(failure: nil))
      print("concrete sum: \(sum)") // CHECK-NEXT: concrete sum: 42
      _ = try await reduceConcrete(TestSequence(failure: IterationError.failed))
    } catch IterationError.failed {
      print("concrete iteration: failed") // CHECK-NEXT: concrete iteration: failed
    } catch {
      print("unexpected error: \(error)")
    }

    do {
      let sum = try await reduceGenericUntyped(TestSequence<IterationError>(failure: nil))
      print("generic sum: \(sum)") // CHECK-NEXT: generic sum: 42
      _ = try await reduceGenericUntyped(TestSequence(failure: IterationError.failed))
    } catch IterationError.failed {
      print("generic iteration: failed") // CHECK-NEXT: generic iteration: failed
    } catch {
      print("unexpected error: \(error)")
    }

    do {
      _ = try await reduceWithThrowingTransform(TestSequence(failure: nil))
    } catch TransformError.failed {
      print("transform failed") // CHECK-NEXT: transform failed
    } catch {
      print("unexpected error: \(error)")
    }

    await discardFailure() // CHECK-NEXT: discarded child completed
    print("discarded failure") // CHECK-NEXT: discarded failure
  }
}
