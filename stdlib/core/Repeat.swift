//===--- Repeat.swift - A Collection that repeats a value N times ---------===//
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
struct Repeat<T> : Collection {
  typealias IndexType = Int

  init(count: Int, repeatedValue: T) {
    self.count = count
    self.repeatedValue = repeatedValue
  }
  
  var startIndex: IndexType {
    return 0
  }
  
  var endIndex: IndexType {
    return count
  }

  func generate() -> IndexingGenerator<Repeat> {
    return IndexingGenerator(self)
  }

  subscript(i: Int) -> T {
    assert(i < count, "Index out of range")
    return repeatedValue
  }

  var count: Int
  let repeatedValue: T
}

