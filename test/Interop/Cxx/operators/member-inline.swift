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
  let arr = ReadOnlyIntArray(1)

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
  let arr = DifferentTypesArray()

  let resultInt: Int32 = arr[2]
  let resultDouble: Double = arr[0.1]

  expectEqual(3, resultInt)
  expectEqual(1.5.rounded(.down), resultDouble.rounded(.down))
  expectEqual(1.5.rounded(.up), resultDouble.rounded(.up))
}

OperatorsTestSuite.test("IntArrayByVal.subscript (inline)") {
  var arr = IntArrayByVal()
  let result0 = arr[0]
  let result1 = arr[1]
  let result2 = arr[2]

  expectEqual(1, result0)
  expectEqual(2, result1)
  expectEqual(3, result2)

  arr.setValueAtIndex(42, 2)
  let result3 = arr[2]
  expectEqual(42, result3)
}

OperatorsTestSuite.test("NonTrivialIntArrayByVal.subscript (inline)") {
  var arr = NonTrivialIntArrayByVal(1)

  let result0 = arr[0]
  let result2 = arr[2]
  let result4 = arr[4]

  expectEqual(1, result0)
  expectEqual(3, result2)
  expectEqual(5, result4)

  arr.setValueAtIndex(42, 2)
  let result5 = arr[2]
  expectEqual(42, result5)
}

OperatorsTestSuite.test("DifferentTypesArrayByVal.subscript (inline)") {
  var arr = DifferentTypesArrayByVal()

  let resultInt: Int32 = arr[2]
  let resultDouble: Double = arr[0.1]

  expectEqual(3, resultInt)
  expectEqual(1.5.rounded(.down), resultDouble.rounded(.down))
  expectEqual(1.5.rounded(.up), resultDouble.rounded(.up))
}

OperatorsTestSuite.test("NonTrivialArrayByVal.subscript (inline)") {
  var arr = NonTrivialArrayByVal()
  let NonTrivialByVal = arr[0];
  let cStr = NonTrivialByVal.Str!
  expectEqual("Non-Trivial", String(cString: cStr))

  expectEqual(1, NonTrivialByVal.a)
  expectEqual(2, NonTrivialByVal.b)
  expectEqual(3, NonTrivialByVal.c)
  expectEqual(4, NonTrivialByVal.d)
  expectEqual(5, NonTrivialByVal.e)
  expectEqual(6, NonTrivialByVal.f)
}

OperatorsTestSuite.test("PtrByVal.subscript (inline)") {
  var arr = PtrByVal()
  expectEqual(64, arr[0]![0])
  arr[0]![0] = 23
  expectEqual(23, arr[0]![0])
}

OperatorsTestSuite.test("ConstOpPtrByVal.subscript (inline)") {
  var arr = ConstOpPtrByVal()
  expectEqual(64, arr[0]![0])
}

OperatorsTestSuite.test("ConstPtrByVal.subscript (inline)") {
  var arr = ConstPtrByVal()
  expectEqual(64, arr[0]![0])
}

OperatorsTestSuite.test("RefToPtr.subscript (inline)") {
  var arr = RefToPtr()
  let ptr: UnsafeMutablePointer<Int32> =
    UnsafeMutablePointer<Int32>.allocate(capacity: 64)
  ptr[0] = 23

  expectEqual(64, arr[0]![0])
  arr[0] = ptr
  expectEqual(23, arr[0]![0])
}

OperatorsTestSuite.test("PtrToPtr.subscript (inline)") {
  var arr = PtrToPtr()
  let ptr: UnsafeMutablePointer<Int32> =
    UnsafeMutablePointer<Int32>.allocate(capacity: 64)
  ptr[0] = 23

  expectEqual(64, arr[0]![0]![0])
  arr[0]![0] = ptr
  expectEqual(23, arr[0]![0]![0])
}

runAllTests()
