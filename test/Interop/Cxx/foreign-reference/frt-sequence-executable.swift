// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default)
// REQUIRES: executable_test

import FrtSequence
import CxxStdlib
import StdlibUnittest

var FrtSequenceTests = TestSuite("Iterating over std::vector of FRTs")

FrtSequenceTests.test("iterate over vector of ImmortalNode pointers") {
  let vec = makeImmortalPtrVector()
  var values: [CInt] = []
  for node in vec {
    values.append(node!.value)
  }
  expectEqual(values, [10, 20, 30])
}

FrtSequenceTests.test("subscript assignment on vector of ImmortalNode pointers") {
  var vec = makeImmortalPtrVector()
  let first = vec[0]
  let second = vec[1]
  // Swap first two elements via subscript assignment.
  vec[0] = second
  vec[1] = first
  expectEqual(vec[0]!.value, 20)
  expectEqual(vec[1]!.value, 10)
  expectEqual(vec[2]!.value, 30)
}

FrtSequenceTests.test("iterate over vector of SharedNode pointers") {
  let vec = makeSharedPtrVector()
  var values: [CInt] = []
  for node in vec {
    values.append(node!.value)
  }
  expectEqual(values, [101, 202, 303])
}

FrtSequenceTests.test("iterate over vector of ImmortalNode2 values") {
  let vec = makeImmortalValVector()
  var values: [CInt] = []
  for node in vec {
    values.append(node.value)
  }
  expectEqual(values, [11, 22, 33])
}

runAllTests()
