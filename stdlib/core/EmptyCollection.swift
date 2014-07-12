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

public struct EmptyGenerator<T> : GeneratorType, SequenceType {
  public func generate() -> EmptyGenerator {
    return self
  }
  
  public mutating func next() -> T? {
    return nil
  }
}

public struct EmptyCollection<T> : CollectionType {
  public typealias Index = Int

  public init() {}
  
  public var startIndex: Index {
    return 0
  }
  
  public var endIndex: Index {
    return 0
  }

  public func generate() -> EmptyGenerator<T> {
    return EmptyGenerator()
  }

  public subscript(i: Index) -> T {
    _preconditionFailure("Index out of range")
  }
}

// Specialization of countElements for EmptyCollection<T>
public func ~> <T>(x:EmptyCollection<T>, _:(_CountElements, ())) -> Int {
  return 0
}
