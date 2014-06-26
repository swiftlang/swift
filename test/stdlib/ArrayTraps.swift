// These tests should crash.
// RUN: mkdir -p %t
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/CatchCrashes.cpp -c -o %t/CatchCrashes.o
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/a.out
//
// RUN: %target-run %t/a.out OutOfBounds1 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out OutOfBounds2 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out OutOfBounds3 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out OutOfBounds4 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out Downcast1 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out Downcast2 2>&1 | FileCheck %s -check-prefix=CHECK

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

if arg == "OutOfBounds4" {
  var a: [Int] = []
  println("OK")
  a.removeLast()
}

class Base { }
class Derived : Base { }

if arg == "Downcast1" {
  let ba: [Base] = [Derived(), Base()]
  // <rdar://problem/17340393> Array downcast should do deferred checking
  // FIXME: The "OK" should be moved after "let d0 = da[0]" when we
  // get deferred checking.
  println("OK")
  let da: [Derived] = _arrayDownCast(ba)
  let d0 = da[0]
  let d1 = da[1]
}

if arg == "Downcast2" {
  let a: [AnyObject] = ["String", 1]
  // <rdar://problem/17340393> Array downcast should do deferred checking
  // FIXME: The "OK" should be moved after "let s0 = sa[0]" when we
  // get deferred checking.
  println("OK")
  let sa: [NSString] = _arrayBridgeFromObjectiveC(a)
  let s0 = sa[0]
  let s1 = sa[1]
}

println("BUSTED: should have crashed already")
exit(1)

