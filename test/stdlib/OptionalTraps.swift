// These tests should crash.
// RUN: true
// UN: mkdir -p %t
// UN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/CatchCrashes.cpp -c -o %t/CatchCrashes.o
// UN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/ReturnNil/ReturnNil.m -c -o %t/ReturnNil.o -g
// UN: %target-build-swift %s -I %S/Inputs/ReturnNil/ -Xlinker %t/CatchCrashes.o -Xlinker %t/ReturnNil.o -o %t/OptionalTraps_Debug
// UN: %target-build-swift %s -I %S/Inputs/ReturnNil/ -Xlinker %t/CatchCrashes.o -Xlinker %t/ReturnNil.o -o %t/OptionalTraps_Release -O
// UN: %target-build-swift %s -I %S/Inputs/ReturnNil/ -Xlinker %t/CatchCrashes.o -Xlinker %t/ReturnNil.o -o %t/OptionalTraps_Fast -Ofast
//
// UN: %target-run %t/OptionalTraps_Debug UnwrapNone1 2>&1 | FileCheck %s -check-prefix=CHECK
// UN: %target-run %t/OptionalTraps_Release UnwrapNone1 2>&1 | FileCheck %s -check-prefix=CHECK
// UN: %target-run %t/OptionalTraps_Fast UnwrapNoneFast1 2>&1 | FileCheck %s -check-prefix=CHECK_UNWRAP_NONE_1_FAST

// CHECK: OK
// CHECK: CRASHED: SIG{{ILL|TRAP}}

import Foundation
import ReturnNil

// Interpret the command line arguments.
var arg = Process.arguments[1]

if arg == "UnwrapNone1" {
  var a: AnyObject? = returnNil()
  println("OK")
  var w = reinterpretCast(a!) as Word
}

if arg == "UnwrapNoneFast1" {
  var a: AnyObject? = returnNil()
  var w = reinterpretCast(a!) as Word
  println("Unwrapping .None: \(w)")
  exit(0)
}
// CHECK_UNWRAP_NONE_1_FAST: Unwrapping .None: 0{{$}}

println("BUSTED: should have crashed already")
exit(1)

