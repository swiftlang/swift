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
  init(_ elements: T?) {
    self.elements = elements
  }

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

  init(_ element: T) { 
    self.element = element 
  }

  var startIndex: IndexType {
    return .zero
  }
  
  var endIndex: IndexType {
    return .one
  }

  func generate() -> GeneratorOfOne<T> {
    return GeneratorOfOne(element)
  }

  subscript(i: IndexType) -> T {
    assert(i == .zero, "Index out of range")
    return element
  }
  
  let element: T
}

// Specialization of countElements for CollectionOfOne<T>
func ~> <T>(x:CollectionOfOne<T>, _:(_CountElements, ())) -> Int {
  return 1
}

  
