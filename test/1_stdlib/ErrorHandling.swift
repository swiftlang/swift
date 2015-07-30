// RUN: %target-run-simple-swift
// REQUIRES: executable_test

//
// Tests for error handling in standard library APIs.
//

import StdlibUnittest

var NoisyCount = 0

class Noisy {
  init() { NoisyCount++ }
  deinit { NoisyCount-- }
}
enum SillyError: ErrorType { case JazzHands }

var ErrorHandlingTests = TestSuite("ErrorHandling")


ErrorHandlingTests.test("ErrorHandling/withUnsafeMutableBufferPointer restores array on throw") {
  var x = [1, 2, 3]
  do {
    // Check that the array buffer is restored when an error is thrown
    // inside withUnsafeMutableBufferPointer
    try x.withUnsafeMutableBufferPointer { p in
      p[0] = 4
      p[1] = 5
      p[2] = 6

      // Buffer should be swapped out of the original array.
      expectEqual(x, [])

      throw SillyError.JazzHands
    }
    expectUnreachable()
  } catch {}

  // Mutated buffer should be restored to the array.
  expectEqual(x, [4, 5, 6])
}

ErrorHandlingTests.test("ErrorHandling/withUnsafeBufferPointer extends lifetime") {
  let initialCount = NoisyCount
  do {
    let x = [Noisy(), Noisy(), Noisy()]
    let countBeforeWithUBP = NoisyCount
    do {
      // Don't use x anywhere in this test after this point.
      try x.withUnsafeBufferPointer { p in
        expectEqual(NoisyCount, countBeforeWithUBP)
        throw SillyError.JazzHands
      }
      expectUnreachable()
    } catch {}
  }
  expectEqual(NoisyCount, initialCount)
}

ErrorHandlingTests.test("ErrorHandling/Optional.map and .flatMap") {
  var x: Int? = 222

  do {
    let y: String? = try x.map {(n: Int) -> String in
      throw SillyError.JazzHands
      return "\(n)"
    }
    expectUnreachable()
  } catch {}

  do {
    let y: String? = try x.flatMap {(n: Int) -> String? in
      throw SillyError.JazzHands
      return .Some("\(n)")
    }
    expectUnreachable()
  } catch {}
}

ErrorHandlingTests.test("ErrorHandling/withCString extends lifetime") {
  do {
    let x = "ad astra per aspera"
    do {
      // Don't use x anywhere in this test after this point.
      try x.withCString { p in
        expectEqual(p[0], Int8(("a" as UnicodeScalar).value))
        expectEqual(p[1], Int8(("d" as UnicodeScalar).value))
        throw SillyError.JazzHands
      }
      expectUnreachable()
    } catch {}
  }
  // TODO: Some way to check string was deallocated?
}

ErrorHandlingTests.test("ErrorHandling/indexOf") {
  do {
    let _: Int? = try [1, 2, 3].indexOf {
      throw SillyError.JazzHands
      return $0 == $0
    }
    expectUnreachable()
  } catch {}
}

runAllTests()
