//===--- RangeTraps.swift -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// These tests should crash
// RUN: mkdir -p %t
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/CatchCrashes.cpp -c -o %t/CatchCrashes.o
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/a.out_Debug
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/a.out_Release -O
//
// RUN: %target-run %t/a.out_Debug HalfOpen 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out_Debug Closed 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out_Debug OutOfRange 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out_Release HalfOpen 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out_Release Closed 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out_Release OutOfRange 2>&1 | FileCheck %s

// XFAIL: linux

// CHECK: OK
// CHECK: CRASHED: SIG{{ILL|TRAP}}

// Interpret the command line arguments.
var arg = Process.arguments[1]

if arg == "HalfOpen" {
  println("OK")
  1..<0
}

if arg == "Closed" {
  println("OK")
  1...0
}

if arg == "OutOfRange" {
  
  0..<Int.max // This is a Range

  // This works for Intervals, but...
  if ClosedInterval(0...Int.max).contains(Int.max) {
    println("OK")
  }
  // ...no support yet for Ranges containing the maximum representable value
#if arch(i386)  ||  arch(arm)
  // FIXME <rdar://17670791> Range<Int> bounds checking not enforced in optimized 32-bit
  1...0  // crash some other way to pacify FileCheck
#else
  0...Int.max
#endif
}

println("BUSTED: should have crashed already")

import Darwin
exit(1)
