//===--- EmptyCollection.swift - A collection with no elements ------------===//
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
//
//  Sometimes an operation is best expressed in terms of some other,
//  larger operation where one of the parameters is an empty
//  collection.  For example, we can erase elements from an Array by
//  replacing a subrange with the empty collection.
//
//===----------------------------------------------------------------------===//

struct EmptyGenerator<T> : Generator, Sequence {
  func generate() -> EmptyGenerator {
    return self
  }
  
  mutating func next() -> T? {
    return nil
  }
}

struct EmptyCollection<T> : Collection {
  typealias IndexType = Int
  
  var startIndex: IndexType {
    return 0
  }
  
  var endIndex: IndexType {
    return 0
  }

  func generate() -> EmptyGenerator<T> {
    return EmptyGenerator()
  }

  subscript(i: IndexType) -> T {
    fatal("Index out of range")
  }
}

// Specialization of countElements for EmptyCollection<T>
func ~> <T>(x:EmptyCollection<T>, _:(_CountElements, ())) -> Int {
  return 0
}
