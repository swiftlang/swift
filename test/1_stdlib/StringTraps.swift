// These tests should crash.
// RUN: mkdir -p %t
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/CatchCrashes.cpp -c -o %t/CatchCrashes.o
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/a.out
//
// RUN: %target-run %t/a.out StringCharacterStartIndexPredecessor 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out StringCharacterEndIndexSuccessor 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out StringCharacterSubscriptEndIndex 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out StringUTF8ViewEndIndexSuccessor 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out StringUTF8ViewSubscriptEndIndex 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out StringUTF16ViewSubscriptDecrementedStartIndex 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out StringUTF16ViewSubscriptEndIndex 2>&1 | FileCheck %s -check-prefix=CHECK

// XFAIL: linux

// CHECK: OK
// CHECK: CRASHED: SIG{{ILL|TRAP|ABRT}}

import Darwin

// Interpret the command line arguments.
var arg = Process.arguments[1]

if arg == "StringCharacterStartIndexPredecessor" {
  var s = "abc"
  var i = s.startIndex
  ++i
  --i
  println("OK")
  --i
}

if arg == "StringCharacterEndIndexSuccessor" {
  var s = "abc"
  var i = s.startIndex
  ++i
  ++i
  ++i
  println("OK")
  ++i
}

if arg == "StringCharacterSubscriptEndIndex" {
  var s = "abc"
  var i = s.startIndex
  ++i
  ++i
  ++i
  println("OK")
  s[i]
}

if arg == "StringUTF8ViewEndIndexSuccessor" {
  var s = "abc"
  var i = s.utf8.startIndex
  ++i
  ++i
  ++i
  println("OK")
  ++i
}

if arg == "StringUTF8ViewSubscriptEndIndex" {
  var s = "abc"
  var i = s.utf8.startIndex
  ++i
  ++i
  ++i
  println("OK")
  s.utf8[i]
}

if arg == "StringUTF16ViewSubscriptDecrementedStartIndex" {
  var s = "abc"
  var i = s.utf16.startIndex
  --i
  println("OK")
  s.utf16[i]
}

if arg == "StringUTF16ViewSubscriptEndIndex" {
  var s = "abc"
  var i = s.utf16.startIndex
  ++i
  ++i
  ++i
  println("OK")
  s.utf16[i]
}

println("BUSTED: should have crashed already")
exit(1)

