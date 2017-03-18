//===--- SequenceWrapper.swift - sequence/collection wrapper protocols ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  To create a Sequence that forwards requirements to an
//  underlying Sequence, have it conform to this protocol.
//
//===----------------------------------------------------------------------===//

/// A type that is just a wrapper over some base Sequence
@_show_in_interface
public // @testable
protocol _SequenceWrapper : Sequence {
  associatedtype Base : Sequence
  associatedtype Iterator : IteratorProtocol = Base.Iterator
  
  var _base: Base { get }
}

extension _SequenceWrapper  {
  public var underestimatedCount: Int {
    return _base.underestimatedCount
  }

  public func _preprocessingPass<R>(
    _ preprocess: () throws -> R
  ) rethrows -> R? {
    return try _base._preprocessingPass(preprocess)
  }
}

extension _SequenceWrapper where Iterator == Base.Iterator {
  public func makeIterator() -> Iterator {
    return self._base.makeIterator()
  }
  
  @discardableResult
  public func _copyContents(
    initializing buf: UnsafeMutableBufferPointer<Iterator.Element>
  ) -> (Iterator, UnsafeMutableBufferPointer<Iterator.Element>.Index) {
    return _base._copyContents(initializing: buf)
  }
}

extension _SequenceWrapper where Iterator.Element == Base.Iterator.Element {
  public func map<T>(
    _ transform: (Iterator.Element) throws -> T
  ) rethrows -> [T] {
    return try _base.map(transform)
  }
  
  public func filter(
    _ isIncluded: (Iterator.Element) throws -> Bool
  ) rethrows -> [Iterator.Element] {
    return try _base.filter(isIncluded)
  }
  
  public func _customContainsEquatableElement(
    _ element: Iterator.Element
  ) -> Bool? { 
    return _base._customContainsEquatableElement(element)
  }
  
  public func _copyToContiguousArray()
    -> ContiguousArray<Iterator.Element> {
    return _base._copyToContiguousArray()
  }
}
