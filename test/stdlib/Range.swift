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

var x: Range<Int> = 3...5

// CHECK-NEXT: 4...6
var y = x + 1
println("\(y.startIndex)...\(y.endIndex)")

// CHECK-NEXT: 5...7
y = 2 + x
println("\(y.startIndex)...\(y.endIndex)")

// CHECK-NEXT: 6...8
x += 3
println("\(x.startIndex)...\(x.endIndex)")

// CHECK-NEXT: 4...6
x -= 2
println("\(x.startIndex)...\(x.endIndex)")

// CHECK-NEXT: done.
println("done.")
