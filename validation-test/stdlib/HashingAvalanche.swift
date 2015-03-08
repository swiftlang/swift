// RUN: %target-build-swift -Xfrontend -disable-access-control -module-name a %s -o %t.out -O
// RUN: %target-run %t.out

import SwiftUnstable
import StdlibUnittest

var HashingTestSuite = TestSuite("Hashing")

func avalancheTest(bits: Int, hashUnderTest: (UInt64) -> UInt64, pValue: Double) {
  let testsInBatch = 100000
  let testData = randArray64(testsInBatch)
  let testDataHashed = Array(lazy(testData).map { hashUnderTest($0) })

  for inputBit in 0..<bits {
    // Using an array here makes the test too slow.
    var bitFlips = UnsafeMutablePointer<Int>.alloc(bits)
    for i in 0..<bits {
      bitFlips[i] = 0
    }
    for i in indices(testData) {
      let inputA = testData[i]
      let outputA = testDataHashed[i]
      let inputB = inputA ^ (1 << UInt64(inputBit))
      let outputB = hashUnderTest(inputB)
      var delta = outputA ^ outputB
      for outputBit in 0..<bits {
        if delta & 1 == 1 {
          ++bitFlips[outputBit]
        }
        delta = delta >> 1
      }
    }
    for outputBit in 0..<bits {
      expectTrue(
        chiSquaredUniform2(testsInBatch, bitFlips[outputBit], pValue)) {
        "inputBit: \(inputBit), outputBit: \(outputBit)"
      }
    }
    bitFlips.dealloc(bits)
  }
}

// White-box testing: assume that the other N-bit to N-bit mixing functions
// just dispatch to these.  (Avalanche test is relatively expensive.)
HashingTestSuite.test("_mixUInt64/avalanche") {
  avalancheTest(64, _mixUInt64, 0.02)
}

HashingTestSuite.test("_mixUInt32/avalanche") {
  avalancheTest(32, { UInt64(_mixUInt32(UInt32($0 & 0xffff_ffff))) }, 0.02)
}

runAllTests()

