// These tests should crash.
// RUN: mkdir -p %t
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/CatchCrashes.cpp -c -o %t/CatchCrashes.o
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/a.out
//
// RUN: %target-run %t/a.out OutOfBounds1 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out OutOfBounds2 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out OutOfBounds3 2>&1 | FileCheck %s -check-prefix=CHECK

// CHECK: OK
// CHECK: CRASHED: SIG{{ILL|TRAP}}

import Foundation

// Interpret the command line arguments.
var arg = Process.arguments[1]

if arg == "OutOfBounds1" {
  var a = Array<Int>()
  println("OK")
  let x = a[0]
}

if arg == "OutOfBounds2" {
  var a = Array<Int>()
  println("OK")
  let x = a[100]
}

if arg == "OutOfBounds3" {
  var a = [ 10, 20, 30 ]
  println("OK")
  let x = a[3]
}

println("BUSTED: should have crashed already")
exit(1)

