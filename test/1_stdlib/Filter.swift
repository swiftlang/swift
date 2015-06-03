//===--- Filter.swift - tests for lazy filtering --------------------------===//
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

// CHECK: testing...
print("testing...")

func printlnByGenerating<S: SequenceType>(s: S) {
  print("<", appendNewline: false)
  var prefix = ""
  for x in s {
    print("\(prefix)\(x)", appendNewline: false)
    prefix = ", "
  }
  print(">")
}

func printlnByIndexing<C: CollectionType>(c: C) {
  printlnByGenerating(
    PermutationGenerator(elements: c, indices: c.indices)
  )
}

// Test filtering Collections
if true {
  let f0 = FilterCollectionView(0..<30) { $0 % 7 == 0 }
  
  // CHECK-NEXT: <0, 7, 14, 21, 28>
  printlnByGenerating(f0)
  // CHECK-NEXT: <0, 7, 14, 21, 28>
  printlnByIndexing(f0)

  // Also try when the first element of the underlying sequence
  // doesn't pass the filter
  let f1 = FilterCollectionView(1..<30) { $0 % 7 == 0 }
  
  // CHECK-NEXT: <7, 14, 21, 28>
  printlnByGenerating(f1)
  // CHECK-NEXT: <7, 14, 21, 28>
  printlnByIndexing(f1)
}


// Test filtering Sequences
if true {
  let f0 = lazy((0..<30).generate()).filter { $0 % 7 == 0 }
  
  // CHECK-NEXT: <0, 7, 14, 21, 28>
  printlnByGenerating(f0)

  // Also try when the first element of the underlying sequence
  // doesn't pass the filter
  let f1 = lazy((1..<30).generate()).filter { $0 % 7 == 0 }
  
  // CHECK-NEXT: <7, 14, 21, 28>
  printlnByGenerating(f1)
}

// CHECK-NEXT: all done.
print("all done.")
