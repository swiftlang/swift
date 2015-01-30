// These tests should crash.
// RUN: mkdir -p %t
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/CatchCrashes.cpp -c -o %t/CatchCrashes.o
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/ReturnNil/ReturnNil.m -c -o %t/ReturnNil.o -g
// RUN: %target-build-swift %s -I %S/Inputs/ReturnNil/ -Xlinker %t/CatchCrashes.o -Xlinker %t/ReturnNil.o -o %t/OptionalTraps_Debug
// RUN: %target-build-swift %s -I %S/Inputs/ReturnNil/ -Xlinker %t/CatchCrashes.o -Xlinker %t/ReturnNil.o -o %t/OptionalTraps_Release -O
// RUN: %target-build-swift %s -I %S/Inputs/ReturnNil/ -Xlinker %t/CatchCrashes.o -Xlinker %t/ReturnNil.o -o %t/OptionalTraps_Fast -Ounchecked
//
// RUN: %target-run %t/OptionalTraps_Debug UnwrapNone1 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/OptionalTraps_Release UnwrapNone1 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/OptionalTraps_Fast UnwrapNoneFast1 2>&1 | FileCheck %s -check-prefix=CHECK_UNWRAP_NONE_1_FAST

// XFAIL: linux

// CHECK: OK
// CHECK: CRASHED: SIG{{ILL|TRAP}}

import Foundation
import ReturnNil

// Interpret the command line arguments.
var arg = Process.arguments[1]

if arg == "UnwrapNone1" {
  var a: AnyObject? = returnNil()
  println("OK")
  var w = unsafeBitCast(a!, Word.self)
}

if arg == "UnwrapNoneFast1" {
  var a: AnyObject? = returnNil()
  var w = unsafeBitCast(a!, Word.self)
  println("Unwrapping .None: \(w)")
  exit(0)
}
// CHECK_UNWRAP_NONE_1_FAST: Unwrapping .None: 0{{$}}

println("BUSTED: should have crashed already")
exit(1)

