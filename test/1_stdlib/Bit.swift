//===--- Bit.swift --------------------------------------------------------===//
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

// REQUIRES: OS=macosx
// FIXME: This fails with r27206 on armv7. rdar://problem/20521110

let zero: Bit = .Zero
let one: Bit = .One

// CHECK: testing
println("testing")

// CHECK-NEXT: 1
println((one - zero).rawValue)
// CHECK-NEXT: 1
println(zero.successor().rawValue)
// CHECK-NEXT: 0
println(one.predecessor().rawValue)

// CHECK-NEXT: 0
println((one &+ one).rawValue)

// CHECK: done.
println("done.")

