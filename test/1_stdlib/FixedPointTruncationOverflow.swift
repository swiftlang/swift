// These tests should crash (until we change overflow trapping mechanizm in stdlib).
// RUN: mkdir -p %t
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/CatchCrashes.cpp -c -o %t/CatchCrashes.o
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/a.out_Debug
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/a.out_Release -O
//
// RUN: %target-run %t/a.out_Debug 1 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out_Debug 2 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out_Debug 3 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out_Debug 4 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out_Debug 5 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out_Debug 6 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out_Debug 7 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out_Debug 8 2>&1 | FileCheck %s -check-prefix=CHECK
//
// RUN: %target-run %t/a.out_Release 1 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out_Release 2 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out_Release 3 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out_Release 4 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out_Release 5 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out_Release 6 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out_Release 7 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out_Release 8 2>&1 | FileCheck %s -check-prefix=CHECK
// XFAIL: linux

// CHECK: OK
// CHECK: CRASHED: SIG{{ILL|TRAP}}

import Darwin
import StdlibUnittest

// Note: in this file, we need to go through opaque functions to load
// constants.  This is to to check runtime behaviour and ensure the constant is
// not folded.

// Interpret the command line arguments.
var arg = Process.arguments[1]

// Test s to s truncation.
if arg == "1" {
  var x = getInt16(128)
  println("OK")
  Int8(x)
}

// Test s to u truncation.
if arg == "2" {
  var x = getInt32(-1)
  println("OK")
  UInt8(x)
}
if arg == "3" {
  var x = getInt32(0xFFFFFFF)
  println("OK")
  UInt16(x)
}

// Unsigned to signed Truncation test.
// Test that we check if we overflow on the sign bit.
if arg == "4" {
  var x = getUInt16(128)
  println("OK")
  Int8(x)
}

// Unsigned to unsigned Truncation test.
if arg == "5" {
  var x = getUInt32(0xFFFFFFFF)
  println("OK")
  UInt16(x)
}

// Same size conversions.
if arg == "6" {
  var x = getInt8(-2)
  println("OK")
  UInt8(x)
}

if arg == "7" {
  var x = getUInt8(128)
  println("OK")
  Int8(x)
}

if arg == "8" {
  var x = getInt8(-128)
  println("OK")
  UInt16(x)
}

println("BUSTED: should have crashed already")
exit(1)
