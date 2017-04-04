// RUN: %target-run-simple-swift
// REQUIRES: executable_test

//
// Tests for error handling in standard library APIs.
//

import StdlibUnittest


var NoisyCount = 0

class Noisy {
  init() { NoisyCount += 1 }
  deinit { NoisyCount -= 1 }
}
enum SillyError : Error { case JazzHands }

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

      // FIXME: Seems to have recently regressed
      // Buffer should be swapped out of the original array.
      // expectEqual(x, [])

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
      return .some("\(n)")
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

ErrorHandlingTests.test("ErrorHandling/index(where:)") {
  do {
    let _: Int? = try [1, 2, 3].index {
      throw SillyError.JazzHands
      return $0 == $0
    }
    expectUnreachable()
  } catch {}
}

ErrorHandlingTests.test("ErrorHandling/split") {
  // Michael NOTE: No idea how this ever compiled first. I thought all splits
  // needed a separator? Disable for now.
  //
  // do {
  //   let _: [String.CharacterView] = try "foo".characters.split { _ in
  //     throw SillyError.JazzHands
  //     return false
  //   }
  //   expectUnreachable()
  // } catch {}
  //
  // do {
  //   let _: [AnySequence<Character>]
  //     = try AnySequence("foo".characters).split { _ in
  //       throw SillyError.JazzHands
  //       return false
  //     }
  //   expectUnreachable()
  // } catch {}
}

ErrorHandlingTests.test("ErrorHandling/forEach") {
  var loopCount = 0
  do {
    try [1, 2, 3].forEach {
      loopCount += 1
      if $0 == 2 {
        throw SillyError.JazzHands
      }
    }
    expectUnreachable()
  } catch {}

  expectEqual(loopCount, 2)
}


ErrorHandlingTests.test("ErrorHandling/Optional flatMap") {
  var loopCount = 0
  do {
    let _: [Int] = try [1, 2, 3].flatMap {
      loopCount += 1
      if $0 == 2 {
        throw SillyError.JazzHands
      }
      return .some($0)
    }
    expectUnreachable()
  } catch {}

  expectEqual(loopCount, 2)
}

ErrorHandlingTests.test("ErrorHandling/Array flatMap") {
  var loopCount = 0
  do {
    let _: [Int] = try [1, 2, 3].flatMap {(x) -> [Int] in
      loopCount += 1
      if x == 2 {
        throw SillyError.JazzHands
      }
      return Array(repeating: x, count: x)
    }
    expectUnreachable()
  } catch {}

  expectEqual(loopCount, 2)
}

ErrorHandlingTests.test("ErrorHandling/min") {
  do {
    let _: Int? = try [1, 2, 3].min { _, _ in
      throw SillyError.JazzHands
      return false
    }
    expectUnreachable()
  } catch {}

  do {
    let _: Int? = try [1, 2, 3].max { _, _ in
      throw SillyError.JazzHands
      return false
    }
    expectUnreachable()
  } catch {}
}

ErrorHandlingTests.test("ErrorHandling/starts(with:)") {
  do {
    let x: Bool = try [1, 2, 3].starts(with: [1, 2]) { _, _ in
      throw SillyError.JazzHands
      return false
    }
    expectUnreachable()
  } catch {}
}

ErrorHandlingTests.test("ErrorHandling/elementsEqual") {
  do {
    let x: Bool = try [1, 2, 3].elementsEqual([1, 2, 3]) { _, _ in
      throw SillyError.JazzHands
      return false
    }
    expectUnreachable()
  } catch {}
}

ErrorHandlingTests.test("ErrorHandling/lexicographicallyPrecedes(_:)") {
  do {
    let x: Bool = try [1, 2, 3].lexicographicallyPrecedes([0, 2, 3]) { _, _ in
      throw SillyError.JazzHands
      return false
    }
    expectUnreachable()
  } catch {}
}

ErrorHandlingTests.test("ErrorHandling/contains") {
  do {
    let x: Bool = try [1, 2, 3].contains { _ in
      throw SillyError.JazzHands
      return false
    }
    expectUnreachable()
  } catch {}
}

ErrorHandlingTests.test("ErrorHandling/reduce") {
  var loopCount = 0
  do {
    let x: Int = try [1, 2, 3, 4, 5].reduce(0) {
      (x: Int, y: Int) -> Int
    in
      loopCount += 1
      var total = x + y
      if total > 5 {
        throw SillyError.JazzHands
      }
      return total
    }
    expectUnreachable()
  } catch {}
  expectEqual(loopCount, 3)
}

func explosiveBoolean() throws -> Bool {
  throw SillyError.JazzHands
}
func explosiveInt() throws -> Int {
  throw SillyError.JazzHands
}

ErrorHandlingTests.test("ErrorHandling/operators") {
  do {
    if try true && explosiveBoolean() {
      expectUnreachable()
    }
    expectUnreachable()
  } catch {}

  do {
    if try false || explosiveBoolean() {
      expectUnreachable()
    }
    expectUnreachable()
  } catch {}

  do {
    if try nil ?? explosiveInt() == 0 {
      expectUnreachable()
    }
    expectUnreachable()
  } catch {}
}

ErrorHandlingTests.test("ErrorHandling/Sequence map") {
  let initialCount = NoisyCount
  let sequence = AnySequence([1, 2, 3])
  for throwAtCount in 0...3 {
    var loopCount = 0
    do {
      let result: [Noisy] = try sequence.map { _ in
        if loopCount == throwAtCount {
          throw SillyError.JazzHands
        }
        loopCount += 1
        return Noisy()
      }
      expectEqual(NoisyCount, initialCount + 3)
      expectEqual(result.count, 3)
    } catch {}
    expectEqual(NoisyCount, initialCount)
  }
}

ErrorHandlingTests.test("ErrorHandling/Sequence filter") {
  let initialCount = NoisyCount
  for condition in [true, false] {
    for throwAtCount in 0...3 {
      let sequence = [Noisy(), Noisy(), Noisy()]
      var loopCount = 0
      do {
        let result: [Noisy] = try sequence.filter { _ in
          if loopCount == throwAtCount {
            throw SillyError.JazzHands
          }
          loopCount += 1
          return condition
        }
        expectEqual(NoisyCount, initialCount + sequence.count)
        expectEqual(result.count, condition ? 3 : 0)
      } catch {}
    }
    expectEqual(NoisyCount, initialCount)
  }
}

ErrorHandlingTests.test("ErrorHandling/Collection map") {
  let initialCount = NoisyCount
  let collection = [1, 2, 3]
  for throwAtCount in 0...3 {
    var loopCount = 0
    do {
      let result: [Noisy] = try collection.map { _ in
        if loopCount == throwAtCount {
          throw SillyError.JazzHands
        }
        loopCount += 1
        return Noisy()
      }
      expectEqual(NoisyCount, initialCount + 3)
      expectEqual(result.count, 3)
    } catch {}
    expectEqual(NoisyCount, initialCount)
  }
}

runAllTests()
