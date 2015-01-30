//===--- IntervalTraps.swift ----------------------------------------------===//
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
// RUN: %target-run %t/a.out_Release HalfOpen 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out_Release Closed 2>&1 | FileCheck %s

// XFAIL: linux

// CHECK: OK
// CHECK: CRASHED: SIG{{ILL|TRAP}}

// Interpret the command line arguments.
var arg = Process.arguments[1]

if arg == "HalfOpen" {
  println("OK")
  1.0..<0.0
}

if arg == "Closed" {
  println("OK")
  1.0...0.0
}

println("BUSTED: should have crashed already")

import Darwin
exit(1)
