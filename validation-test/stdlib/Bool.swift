// RUN: mkdir -p %t
// RUN: %target-clang -x c %S/Inputs/VariadicBool/variadicBool.c -c -o %t/variadicBool.o
// RUN: %target-build-swift -I %S/Inputs/VariadicBool/ %t/variadicBool.o %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test


import Swift
import StdlibUnittest
import StdlibCollectionUnittest
import VariadicBool

func countTrues(_ count: Int, _ bools: CVarArg...) -> Int {
  return Int(withVaList(bools) { numberOfTrues(Int32(count), $0) })
}

var BoolTestSuite = TestSuite("Bool")

BoolTestSuite.test("AllTrues") {
  let result = countTrues(2, true, true)
  expectEqual(2, result)
}

BoolTestSuite.test("HalfAndHalf") {
  let result = countTrues(4, true, true, false, false)
  expectEqual(2, result)
}

BoolTestSuite.test("AllFalse") {
  let result = countTrues(2, false, false)
  expectEqual(0, result)
}

BoolTestSuite.test("Interleaved") {
  let result = countTrues(7, false, true, false, true, false, true, false)
  expectEqual(3, result)
}

BoolTestSuite.test("FalsePositive") {
  let result = countTrues(7, false, true, false, true, false, true, false)
  expectNotEqual(30, result)
}

BoolTestSuite.test("Toggle") {
  var result = [false, true, true]
  result[0].toggle()
  result[1].toggle()
  expectEqual([true, false, true], result)
}


runAllTests()
