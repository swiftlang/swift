// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test
//
// We can't yet call member functions correctly on Windows (SR-13129).
// XFAIL: OS=windows-msvc

import MemberInline
import StdlibUnittest

var OperatorsTestSuite = TestSuite("Operators")

OperatorsTestSuite.test("LoadableIntWrapper.plus (inline)") {
  var lhs = LoadableIntWrapper(value: 42)
  let rhs = LoadableIntWrapper(value: 23)

  let result = lhs - rhs

  expectEqual(19, result.value)
}

OperatorsTestSuite.test("LoadableIntWrapper.call (inline)") {
  var wrapper = LoadableIntWrapper(value: 42)

  let resultNoArgs = wrapper()
  let resultOneArg = wrapper(23)
  let resultTwoArgs = wrapper(3, 5)

  expectEqual(42, resultNoArgs)
  expectEqual(65, resultOneArg)
  expectEqual(57, resultTwoArgs)
}

OperatorsTestSuite.test("AddressOnlyIntWrapper.call (inline)") {
  var wrapper = AddressOnlyIntWrapper(42)

  let resultNoArgs = wrapper()
  let resultOneArg = wrapper(23)
  let resultTwoArgs = wrapper(3, 5)

  expectEqual(42, resultNoArgs)
  expectEqual(65, resultOneArg)
  expectEqual(57, resultTwoArgs)
}

OperatorsTestSuite.test("ReadWriteIntArray.subscript (inline)") {
  var arr = ReadWriteIntArray()

  let resultBefore = arr[1]
  expectEqual(2, resultBefore)

  arr[1] = 234

  let resultAfter = arr[1]
  expectEqual(234, resultAfter)
}

OperatorsTestSuite.test("ReadOnlyIntArray.subscript (inline)") {
  var arr = ReadOnlyIntArray(1)

  let result0 = arr[0]
  let result2 = arr[2]
  let result4 = arr[4]

  expectEqual(1, result0)
  expectEqual(3, result2)
  expectEqual(5, result4)
}

OperatorsTestSuite.test("WriteOnlyIntArray.subscript (inline)") {
  var arr = WriteOnlyIntArray()

  let resultBefore = arr[0]
  expectEqual(1, resultBefore)

  arr[0] = 654

  let resultAfter = arr[0]
  expectEqual(654, resultAfter)
}

OperatorsTestSuite.test("DifferentTypesArray.subscript (inline)") {
  var arr = DifferentTypesArray()

  let resultInt: Int32 = arr[2]
  let resultDouble: Double = arr[0.1]

  expectEqual(3, resultInt)
  expectEqual(1.5.rounded(.down), resultDouble.rounded(.down))
  expectEqual(1.5.rounded(.up), resultDouble.rounded(.up))
}

runAllTests()
