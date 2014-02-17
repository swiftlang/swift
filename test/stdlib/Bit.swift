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

let zero: Bit = .zero
let one: Bit = .one

// CHECK: testing
println("testing")

// CHECK-NEXT: 1
println((one - zero).toRaw())
// CHECK-NEXT: 1
println(zero.succ().toRaw())
// CHECK-NEXT: 0
println(one.pred().toRaw())

// CHECK: done.
println("done.")

