// These tests should crash (until we change overflow trapping mechanizm in stdlib).
// RUN: mkdir -p %t
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/CatchCrashes.cpp -c -o %t/CatchCrashes.o
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/a.out
//
// RUN: %target-run %t/a.out 1 2>&1 | FileCheck %s -check-prefix=CHECK_1
// RUN: %target-run %t/a.out 2 2>&1 | FileCheck %s -check-prefix=CHECK_2
// RUN: %target-run %t/a.out 3 2>&1 | FileCheck %s -check-prefix=CHECK_3
// RUN: %target-run %t/a.out 4 2>&1 | FileCheck %s -check-prefix=CHECK_4
// RUN: %target-run %t/a.out 5 2>&1 | FileCheck %s -check-prefix=CHECK_5
// RUN: %target-run %t/a.out 6 2>&1 | FileCheck %s -check-prefix=CHECK_6
// RUN: %target-run %t/a.out 7 2>&1 | FileCheck %s -check-prefix=CHECK_7
// RUN: %target-run %t/a.out 8 2>&1 | FileCheck %s -check-prefix=CHECK_8

import Darwin

// Interpret the command line arguments.
var arg = Process.arguments[1]

// Test s to s truncation.
// We need to go through the function to check runtime behaviour and ensure the constant is not folded.
func getInt1() -> Int16 {
  return 128
}
if arg == "1" {
  var x : Int16 = getInt1()
  println("OK")
  // CHECK_1: OK
  Int8(x)
  // CHECK_1-NEXT: CRASHED: SIG{{ILL|TRAP}}
}

// Test s to u truncation.
func getInt2() -> Int32 {
  return -1
}
if arg == "2" {
  var x: Int32 = getInt2()
  println("OK")
  // CHECK_2: OK
  UInt8(x)
  // CHECK_2-NEXT: CRASHED: SIG{{ILL|TRAP}}
}
func getInt3() -> Int32 {
  return 0xFFFFFFF
}
if arg == "3" {
  var x: Int32 = getInt3()
  println("OK")
  // CHECK_3: OK
  UInt16(x)
  // CHECK_3-NEXT: CRASHED: SIG{{ILL|TRAP}}
}

// Unsigned to signed Truncation test.
// Test that we check if we overflow on the sign bit.
func getInt4() -> UInt16 {
  return 128
}
if arg == "4" {
  var y : UInt16 = getInt4()
  println("OK")
  // CHECK_4: OK
  Int8(y)
  // CHECK_4-NEXT: CRASHED: SIG{{ILL|TRAP}}
}

// Unsigned to unsigned Truncation test.
func getInt5() -> UInt32 {
  return 0xFFFFFFFF
}
if arg == "5" {
  var x : UInt32 = getInt5()
  println("OK")
  // CHECK_5: OK
  UInt16(x)
  // CHECK_5-NEXT: CRASHED: SIG{{ILL|TRAP}}
}

// Same size conversions.
func getInt6() -> Int8 {
  return -2
}
if arg == "6" {
  var x : Int8 = getInt6()
  println("OK")
  // CHECK_6: OK
  UInt8(x)
  // CHECK_6-NEXT: CRASHED: SIG{{ILL|TRAP}}
}

func getInt7() -> UInt8 {
  return 128
}
if arg == "7" {
  var x : UInt8 = getInt7()
  println("OK")
  // CHECK_7: OK
  Int8(x)
  // CHECK_7-NEXT: CRASHED: SIG{{ILL|TRAP}}
}

func getInt8() -> Int8 {
  return -128
}
if arg == "8" {
  var x : Int8 = getInt8()
  println("OK")
  // CHECK_8: OK
  UInt16(x)
  // CHECK_8-NEXT: CRASHED: SIG{{ILL|TRAP}}
}

println("BUSTED: should have crashed already")
exit(1)
