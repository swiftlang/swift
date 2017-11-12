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
  associatedtype Base : Sequence where Base.Element == Element
  associatedtype Iterator = Base.Iterator
  associatedtype SubSequence = Base.SubSequence
  
  var _base: Base { get }
}

extension _SequenceWrapper  {
  @_inlineable // FIXME(sil-serialize-all)
  public var underestimatedCount: Int {
    return _base.underestimatedCount
  }

  @_inlineable // FIXME(sil-serialize-all)
  public func _preprocessingPass<R>(
    _ preprocess: () throws -> R
  ) rethrows -> R? {
    return try _base._preprocessingPass(preprocess)
  }
}

extension _SequenceWrapper where Iterator == Base.Iterator {
  @_inlineable // FIXME(sil-serialize-all)
  public func makeIterator() -> Iterator {
    return self._base.makeIterator()
  }
  
  @_inlineable // FIXME(sil-serialize-all)
  @discardableResult
  public func _copyContents(
    initializing buf: UnsafeMutableBufferPointer<Element>
  ) -> (Iterator, UnsafeMutableBufferPointer<Element>.Index) {
    return _base._copyContents(initializing: buf)
  }
}

extension _SequenceWrapper {
  @_inlineable // FIXME(sil-serialize-all)
  public func map<T>(
    _ transform: (Element) throws -> T
) rethrows -> [T] {
    return try _base.map(transform)
  }
  
  @_inlineable // FIXME(sil-serialize-all)
  public func filter(
    _ isIncluded: (Element) throws -> Bool
  ) rethrows -> [Element] {
    return try _base.filter(isIncluded)
  }

  @_inlineable // FIXME(sil-serialize-all)
  public func forEach(_ body: (Element) throws -> Void) rethrows {
    return try _base.forEach(body)
  }
  
  @_inlineable // FIXME(sil-serialize-all)
  public func _customContainsEquatableElement(
    _ element: Element
  ) -> Bool? { 
    return _base._customContainsEquatableElement(element)
  }
  
  @_inlineable // FIXME(sil-serialize-all)
  public func _copyToContiguousArray()
    -> ContiguousArray<Element> {
    return _base._copyToContiguousArray()
  }
}

extension _SequenceWrapper where SubSequence == Base.SubSequence {
  @_inlineable // FIXME(sil-serialize-all)
  public func dropFirst(_ n: Int) -> SubSequence {
    return _base.dropFirst(n)
  }
  @_inlineable // FIXME(sil-serialize-all)
  public func dropLast(_ n: Int) -> SubSequence {
    return _base.dropLast(n)
  }
  @_inlineable // FIXME(sil-serialize-all)
  public func prefix(_ maxLength: Int) -> SubSequence {
    return _base.prefix(maxLength)
  }
  @_inlineable // FIXME(sil-serialize-all)
  public func suffix(_ maxLength: Int) -> SubSequence {
    return _base.suffix(maxLength)
  }

  @_inlineable // FIXME(sil-serialize-all)
  public func drop(
    while predicate: (Element) throws -> Bool
  ) rethrows -> SubSequence {
    return try _base.drop(while: predicate)
  }

  @_inlineable // FIXME(sil-serialize-all)
  public func prefix(
    while predicate: (Element) throws -> Bool
  ) rethrows -> SubSequence {
    return try _base.prefix(while: predicate)
  }
  
  @_inlineable // FIXME(sil-serialize-all)
  public func split(
    maxSplits: Int, omittingEmptySubsequences: Bool,
    whereSeparator isSeparator: (Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    return try _base.split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmptySubsequences,
      whereSeparator: isSeparator
    )
  }
}
