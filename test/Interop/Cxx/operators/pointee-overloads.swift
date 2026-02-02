// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default)
//
// REQUIRES: executable_test

import PointeeOverloads
import StdlibUnittest

var PointeeOverloadTestSuite = TestSuite("OperatorOverload")

PointeeOverloadTestSuite.test("Pointee_Const") {
  let a = Pointee_Const()
  expectEqual(a.pointee, 111)

  var b = Pointee_Const()
  expectEqual(b.pointee, 111)
  b.x = 100
  expectEqual(b.pointee, 100)
}

PointeeOverloadTestSuite.test("Pointee_NonConst") {
  var a = Pointee_NonConst()
  expectEqual(a.pointee, 222)
  a.pointee = 200
  expectEqual(a.x, 200)
  a.x = 202
  expectEqual(a.pointee, 202)
}

PointeeOverloadTestSuite.test("Pointee_Const_NonConst") {
  let a = Pointee_Const_NonConst()
  expectEqual(a.pointee, 333)

  var b = Pointee_Const_NonConst()
  expectEqual(b.pointee, 333)
  b.pointee = 400
  expectEqual(b.pointee, 333)
  expectEqual(b.x, 333)
  expectEqual(b.y, 400)
  b.y = 440
  expectEqual(b.pointee, 333)
  expectEqual(b.x, 333)
  expectEqual(b.y, 440)
}

PointeeOverloadTestSuite.test("Pointee_NonConst_Const") {
  let a = Pointee_NonConst_Const()
  expectEqual(a.pointee, 333)

  var b = Pointee_NonConst_Const()
  expectEqual(b.pointee, 333)
  b.pointee = 400
  expectEqual(b.pointee, 333)
  expectEqual(b.x, 333)
  expectEqual(b.y, 400)
}

PointeeOverloadTestSuite.test("Pointee_NonConst_NonConst") {
  let a = Pointee_NonConst_NonConst()
  expectEqual(a.pointee, 333)

  var b = Pointee_NonConst_NonConst()
  expectEqual(b.pointee, 333)
}

PointeeOverloadTestSuite.test("Pointee_Volatile_Const") {
  var a = Pointee_Volatile_Const()
  expectEqual(a.pointee, 555)
}

PointeeOverloadTestSuite.test("Pointee_NonConstGetter") {
  var a = Pointee_NonConstGetter()
  expectEqual(a.pointee, 666)
  // a.pointee = 6466 // get-only property
}

PointeeOverloadTestSuite.test("Pointee_MutableConst") {
  let a = Pointee_MutableConst() // for some reason let is ok here
  expectEqual(a.pointee, 666)

  a.pointee = 600 // FIXME: assigning to mutable doesn't seem to work
  expectNotEqual(a.pointee, 600) // FIXME: this should be expectEqual
  expectNotEqual(a.x, 600) // FIXME: this should be expectEqual
}

PointeeOverloadTestSuite.test("Pointee_LConst") {
  let a = Pointee_LConst()
  expectEqual(a.pointee, 1111)

  var b = Pointee_LConst()
  expectEqual(b.pointee, 1111)
  b.x = 1000
  expectEqual(b.pointee, 1000)
}

PointeeOverloadTestSuite.test("Pointee_LNonConst") {
  var a = Pointee_LNonConst()
  expectEqual(a.pointee, 2222)
  a.pointee = 2000
  expectEqual(a.x, 2000)
  a.x = 2020
  expectEqual(a.pointee, 2020)
}

PointeeOverloadTestSuite.test("Pointee_LConst_LNonConst") {
  let a = Pointee_LConst_LNonConst()
  expectEqual(a.pointee, 3333)

  var b = Pointee_LConst_LNonConst()
  expectEqual(b.pointee, 3333)
  b.pointee = 4000
  expectEqual(b.pointee, 3333)
  expectEqual(b.x, 3333)
  expectEqual(b.y, 4000)
}

PointeeOverloadTestSuite.test("Pointee_LNonConst_LConst") {
  let a = Pointee_LConst_LNonConst()
  expectEqual(a.pointee, 3333)

  var b = Pointee_LConst_LNonConst()
  expectEqual(b.pointee, 3333)
  b.pointee = 4000
  expectEqual(b.pointee, 3333)
  expectEqual(b.x, 3333)
  expectEqual(b.y, 4000)
  b.y = 4400
  expectEqual(b.pointee, 3333)
  expectEqual(b.x, 3333)
  expectEqual(b.y, 4400)
}

PointeeOverloadTestSuite.test("Pointee_LConst_RConst") {
  let a = Pointee_LConst_RConst()
  expectEqual(a.pointee, 5555)

  var b = Pointee_LConst_RConst()
  expectEqual(b.pointee, 5555)
  b.x = 5000
  b.y = 6000
  expectEqual(b.pointee, 5000)
}

PointeeOverloadTestSuite.test("Pointee_LNonConst_RNonConst") {
  var a = Pointee_LNonConst_RNonConst()
  expectEqual(a.pointee, 5555)
  a.pointee = 5000
  expectEqual(a.x, 5000)
  expectEqual(a.y, 6666)
  a.x = 5050
  a.y = 6060
  expectEqual(a.pointee, 5050)
  expectEqual(a.x, 5050)
  expectEqual(a.y, 6060)
}

runAllTests()
