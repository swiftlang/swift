// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=linux-gnu

import StdlibUnittest
import CustomSequence
import Cxx

var CxxSequenceTestSuite = TestSuite("CxxSequence")

CxxSequenceTestSuite.test("SimpleSequence as Swift.Sequence") {
  let seq = SimpleSequence()
  let contains = seq.contains(where: { $0 == 3 })
  expectTrue(contains)

  var items: [Int32] = []
  for item in seq {
    items.append(item)
  }
  expectEqual([1, 2, 3, 4] as [Int32], items)
}

extension ComparableIterator : Comparable {}
extension SimpleCollection : CxxCollection {}

CxxSequenceTestSuite.test("SimpleCollection as Swift.Collection") {
  let seq = SimpleCollection()
  expectEqual(seq.count, 4)
  expectEqual(seq.first!, 1)

  let secondIndex = seq.index(after: seq.startIndex)
  expectEqual(seq[secondIndex], 2)

  expectEqual([1, 2, 3, 4] as [Int32], seq.map { $0 })
}

CxxSequenceTestSuite.test("SimpleEmptySequence as Swift.Sequence") {
  let seq = SimpleEmptySequence()

  var iterated = false
  for _ in seq {
    iterated = true
  }
  expectFalse(iterated)
}

runAllTests()
