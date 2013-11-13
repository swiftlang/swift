// RUN: not --crash %swift -i %s --args 1 2>&1 | FileCheck %s -check-prefix=CHECK_1
// RUN: not --crash %swift -i %s --args 2 2>&1 | FileCheck %s -check-prefix=CHECK_2
// RUN: not --crash %swift -i %s --args 3 2>&1 | FileCheck %s -check-prefix=CHECK_3
// RUN: not --crash %swift -i %s --args 4 2>&1 | FileCheck %s -check-prefix=CHECK_4
// RUN: not --crash %swift -i %s --args 5 2>&1 | FileCheck %s -check-prefix=CHECK_5
// REQUIRES: swift_interpreter

// These tests should crash (until we change overflow trapping mechanizm in stdlib).

// Interpret the command line arguments.
var arg = Process.arguments[0]

// Test s to s truncation.
// We need to go through the function to check runtime behaviour and ensure the constant is not folded.
def getInt1() -> Int16 {
  return 128
}
if arg == "1" {
  var x : Int16 = getInt1()
  con.write("OK")
  // CHECK_1: OK
  Int8(x)
}

// Test s to u truncation.
def getInt2() -> Int32 {
  return -1
}
if arg == "2" {
  var x: Int32 = getInt2()
  con.write("OK")
  // CHECK_2: OK
  UInt8(x)
}
def getInt3() -> Int32 {
  return 0xFFFFFFF
}
if arg == "3" {
  var x: Int32 = getInt3()
  con.write("OK")
  // CHECK_3: OK
  UInt16(x)
}

// Unsigned to signed Truncation test.
// Test that we check if we overflow on the sign bit.
def getInt4() -> UInt16 {
  return 128
}
if arg == "4" {
  var y : UInt16 = getInt4()
  con.write("OK")
  // CHECK_4: OK
  Int8(y)
}

// Unsigned to unsigned Truncation test.
def getInt5() -> UInt32 {
  return 0xFFFFFFFF
}
if arg == "5" {
  var x : UInt32 = getInt5()
  con.write("OK")
  // CHECK_5: OK
  UInt16(x)
}
