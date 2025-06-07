// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)
//
// REQUIRES: executable_test

import StdlibUnittest
import CustomSequence
import Cxx

var CxxSequenceTestSuite = TestSuite("CxxConvertibleToCollection")

CxxSequenceTestSuite.test("SimpleSequence to Swift.Array") {
  let seq = SimpleSequence()
  let array = Array(seq)
  expectEqual([1, 2, 3, 4] as [Int32], array)
}

CxxSequenceTestSuite.test("SimpleCopyAwareSequence to Swift.Array") {
  copiesCount = 0

  let seq = SimpleCopyAwareSequence()
  let array = Array(seq)

  expectEqual(0, copiesCount) // make sure we don't copy the C++ sequence value unnecessarily
}

CxxSequenceTestSuite.test("SimpleSequenceWithOutOfLineEqualEqual to Swift.Array") {
  let seq = SimpleSequenceWithOutOfLineEqualEqual()
  let array = Array(seq)
  expectEqual([1, 2, 3, 4] as [Int32], array)
}

CxxSequenceTestSuite.test("SimpleSequence to Swift.Set") {
  let seq = SimpleSequence()
  let set = Set(seq)
  expectEqual(Set([1, 2, 3, 4] as [Int32]), set)
}

CxxSequenceTestSuite.test("SimpleEmptySequence to Swift.Array") {
  let seq = SimpleEmptySequence()
  let array = Array(seq)
  expectTrue(array.isEmpty)
}

CxxSequenceTestSuite.test("SimpleEmptySequence to Swift.Set") {
  let seq = SimpleEmptySequence()
  let set = Set(seq)
  expectTrue(set.isEmpty)
}

CxxSequenceTestSuite.test("SimpleSequence.forEach") {
  let seq = SimpleSequence()
  var array: [Int32] = []
  seq.forEach {
    array.append($0)
  }
  expectEqual([1, 2, 3, 4] as [Int32], array)
}

CxxSequenceTestSuite.test("HasInheritedConstIterator to Swift.Array") {
  let seq = HasInheritedConstIterator()
  let array = Array(seq)
  expectEqual([1, 2, 3, 4, 5] as [Int32], array)
}

CxxSequenceTestSuite.test("HasInheritedTemplatedConstIterator to Swift.Array") {
  let seq = HasInheritedTemplatedConstIteratorInt()
  let array = Array(seq)
  expectEqual([1, 2, 3, 4, 5, 6] as [Int32], array)
}

CxxSequenceTestSuite.test("HasInputOutputConstIterator to Swift.Array") {
  let seq = HasInputOutputConstIterator()
  let array = Array(seq)
  expectEqual([5, 4, 3, 2, 1] as [Int32], array)
}

runAllTests()
