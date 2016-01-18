//===--- CollectionOfOne.swift - Tests ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

print("testing...")
// CHECK: testing...

print("for loop: ", terminator: "")
for x in CollectionOfOne(2) {
  print(x, terminator: "")
}
print(".")
// CHECK-NEXT: for loop: 2.

let twentyOne = CollectionOfOne(21)

print("index loop: ", terminator: "")
for x in twentyOne.indices {
  print(twentyOne[x] * 2, terminator: "")
}
print(".")
// CHECK-NEXT: index loop: 42.

print("done.")
// CHECK-NEXT: done.

