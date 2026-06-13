// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: concurrency

// FIXME: wasi-wasm32 traps in Result<T, any Error>'s value-witness copy
// after returning from the new Result.init(catching:) async.
// XFAIL: OS=wasip1
// https://github.com/swiftlang/swift/issues/89155

import StdlibUnittest
import Swift

let ResultAsyncInitTests = TestSuite("ResultAsyncInit")

fileprivate enum Err: Error, Equatable {
  case err
  case derr
}

fileprivate let string = "string"

fileprivate extension Result {
  var success: Success? {
    switch self {
    case let .success(success):
      return success
    case .failure:
      return nil
    }
  }

  var failure: Failure? {
    switch self {
    case .success:
      return nil
    case let .failure(failure):
      return failure
    }
  }
}

ResultAsyncInitTests.test("Async Initialization") {
  func asyncThrowing() async throws -> String {
    throw Err.err
  }

  func asyncNotThrowing() async throws -> String {
    return string
  }

  let result1 = await Result { try await asyncThrowing() }
  let result2 = await Result { try await asyncNotThrowing() }

  expectEqual(result1.failure as? Err, Err.err)
  expectEqual(result2.success, string)

  do {
    _ = try result1.get()
  } catch let error as Err {
    expectEqual(error, Err.err)
  } catch {
    expectUnreachable()
  }

  do {
    let unwrapped = try result2.get()
    expectEqual(unwrapped, string)
  } catch {
    expectUnreachable()
  }
}

await runAllTestsAsync()
