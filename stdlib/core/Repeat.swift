//===--- Repeat.swift - A CollectionType that repeats a value N times -----===//
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
public struct Repeat<T> : CollectionType {
  public typealias Index = Int

  public init(count: Int, repeatedValue: T) {
    self.count = count
    self.repeatedValue = repeatedValue
  }
  
  public var startIndex: Index {
    return 0
  }
  
  public var endIndex: Index {
    return count
  }

  public func generate() -> IndexingGenerator<Repeat> {
    return IndexingGenerator(self)
  }

  public subscript(i: Int) -> T {
    _precondition(i < count, "Index out of range")
    return repeatedValue
  }

  public var count: Int
  public let repeatedValue: T
}

