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

@public struct GeneratorOfOne<T> : Generator, Sequence {
  @public init(_ elements: T?) {
    self.elements = elements
  }

  @public func generate() -> GeneratorOfOne {
    return self
  }
  
  @public mutating func next() -> T? {
    let result = elements
    elements = .None
    return result
  }
  var elements: T?
}

@public struct CollectionOfOne<T> : Collection {
  @public typealias IndexType = Bit

  @public init(_ element: T) { 
    self.element = element 
  }

  @public var startIndex: IndexType {
    return .zero
  }
  
  @public var endIndex: IndexType {
    return .one
  }

  @public func generate() -> GeneratorOfOne<T> {
    return GeneratorOfOne(element)
  }

  @public subscript(i: IndexType) -> T {
    _precondition(i == .zero, "Index out of range")
    return element
  }
  
  let element: T
}

// Specialization of countElements for CollectionOfOne<T>
@public func ~> <T>(x:CollectionOfOne<T>, _:(_CountElements, ())) -> Int {
  return 1
}
