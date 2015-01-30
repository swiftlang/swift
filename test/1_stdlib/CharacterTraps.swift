// These tests should crash.
// RUN: mkdir -p %t
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/CatchCrashes.cpp -c -o %t/CatchCrashes.o
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/a.out
//
// RUN: %target-run %t/a.out CharacterFromEmptyString 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out CharacterFromMoreThanOneGraphemeCluster 2>&1 | FileCheck %s -check-prefix=CHECK

// CHECK: OK
// CHECK: CRASHED: SIG{{ILL|TRAP|ABRT}}

// XFAIL: linux

import Darwin

// Interpret the command line arguments.
var arg = Process.arguments[1]

if arg == "CharacterFromEmptyString" {
  var s = ""
  println("OK")
  Character(s)
}

if arg == "CharacterFromMoreThanOneGraphemeCluster" {
  var s = "ab"
  println("OK")
  Character(s)
}

println("BUSTED: should have crashed already")
exit(1)

