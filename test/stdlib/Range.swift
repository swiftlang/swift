//===--- Range.swift ------------------------------------------------------===//
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
// RUN: %target-run-simple-swift | FileCheck %s

// CHECK: testing...
println("testing...")

var x: Range<Int> = 3...10

// CHECK-NEXT: 3579.
for a in x.by(2) {
  print(a)
}
println(".")

for i in 1.4 .. 3.4 { println(i) }
// CHECK-NEXT: 1.4
// CHECK-NEXT: 2.4
// CHECK-NEXT: 3.4




// CHECK-NEXT: done.
println("done.")
