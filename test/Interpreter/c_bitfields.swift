// RUN: %empty-directory(%t)
// RUN: %target-build-swift -I %S/../Inputs/clang-importer-sdk/platform/any/usr/include %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import StdlibUnittest


import ctypes

var BitfieldTestSuite = TestSuite("Bitfields")

BitfieldTestSuite.test("Simple") {
  var new: ModRM = ModRM()
  new.rm = 7
  new.reg = 5
  new.mod = 3
  new.opcode = 44

  expectEqual(7, new.rm)
  expectEqual(5, new.reg)
  expectEqual(3, new.mod)
  expectEqual(44, new.opcode)
}

BitfieldTestSuite.test("Anonymous Struct") {
  var new = AnonStructWithBitfields()
  new.left.first = 1
  new.left.second = -1
  new.right.first = -1
  new.right.second = 1

  expectEqual(1, new.left.first)
  expectEqual(-1, new.left.second)
  expectEqual(-1, new.right.first)
  expectEqual(1, new.right.second)
}

BitfieldTestSuite.test("Initializer") {
  let new = ModRM(rm: 6, reg: 4, mod: 2, opcode: 33)

  expectEqual(6, new.rm)
  expectEqual(4, new.reg)
  expectEqual(2, new.mod)
  expectEqual(33, new.opcode)
}

runAllTests()
