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
  associatedtype Iterator = Base.Iterator
  associatedtype SubSequence = Base.SubSequence
  
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

  public func forEach(_ body: (Iterator.Element) throws -> Void) rethrows {
    return try _base.forEach(body)
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

extension _SequenceWrapper where SubSequence == Base.SubSequence {
  public func dropFirst(_ n: Int) -> SubSequence {
    return _base.dropFirst(n)
  }
  public func dropLast(_ n: Int) -> SubSequence {
    return _base.dropLast(n)
  }
  public func prefix(_ maxLength: Int) -> SubSequence {
    return _base.prefix(maxLength)
  }
  public func suffix(_ maxLength: Int) -> SubSequence {
    return _base.suffix(maxLength)
  }
}

extension _SequenceWrapper
  where SubSequence == Base.SubSequence, Iterator.Element == Base.Iterator.Element {

  public func drop(
    while predicate: (Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    return try _base.drop(while: predicate)
  }

  public func prefix(
    while predicate: (Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    return try _base.prefix(while: predicate)
  }
  
  public func split(
    maxSplits: Int, omittingEmptySubsequences: Bool,
    whereSeparator isSeparator: (Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    return try _base.split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmptySubsequences,
      whereSeparator: isSeparator
    )
  }
}
