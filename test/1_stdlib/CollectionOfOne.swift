//===--- CollectionOfOne.swift - Tests ------------------------------------===//
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

println("testing...")
// CHECK: testing...

print("for loop: ")
for x in CollectionOfOne(2) {
  print(x)
}
println(".")
// CHECK-NEXT: for loop: 2.

let twentyOne = CollectionOfOne(21)

print("index loop: ")
for x in twentyOne.indices {
  print(twentyOne[x] * 2)
}
println(".")
// CHECK-NEXT: index loop: 42.

println("done.")
// CHECK-NEXT: done.

