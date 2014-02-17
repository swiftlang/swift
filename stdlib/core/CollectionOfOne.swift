//===--- CollectionOfOne.swift - A Collection with one element ------------===//
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

struct GeneratorOfOne<T> : Generator, Sequence {
  func generate() -> GeneratorOfOne {
    return self
  }
  
  mutating func next() -> T? {
    let result = elements
    elements = .None
    return result
  }
  
  var elements: T?
}

struct CollectionOfOne<T> : Collection {
  typealias IndexType = Bit
  
  func startIndex() -> IndexType {
    return .zero
  }
  
  func endIndex() -> IndexType {
    return .one
  }

  func generate() -> GeneratorOfOne<T> {
    return GeneratorOfOne(element)
  }

  func __getitem__(i: IndexType) -> T {
    assert(i == .zero, "Index out of range")
    return element
  }

  subscript(i: IndexType) -> T {
    return __getitem__(i)
  }
  
  val element: T
}
