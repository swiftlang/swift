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
// REQUIRES: executable_test

let zero: Bit = .Zero
let one: Bit = .One

// CHECK: testing
print("testing")

// CHECK-NEXT: 1
print((one - zero).rawValue)
// CHECK-NEXT: 1
print(zero.successor().rawValue)
// CHECK-NEXT: 0
print(one.predecessor().rawValue)

// CHECK-NEXT: 0
print((one &+ one).rawValue)

// CHECK: done.
print("done.")

