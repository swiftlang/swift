//===--- FlatMap.swift ----------------------------------------------------===//
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

extension _prext_LazySequenceType where
Generator.Element == Elements.Generator.Element {
  /// Returns the concatenated results of mapping `transform` over
  /// `self`.  Equivalent to 
  ///
  ///     self.map(transform).flatten
  ///
  /// - Complexity: O(1)
  public func flatMap<Intermediate: SequenceType>(
    transform: (Generator.Element)->Intermediate
  ) -> _prext_LazySequence<
    _prext_FlattenSequence<_prext_LazyMapSequence<Elements, Intermediate>>> {
    return self.map(transform)._prext_flatten
  }
}

extension _prext_LazyCollectionType
where Generator.Element == Elements.Generator.Element{
  /// Returns the concatenated results of mapping `transform` over
  /// `self`.  Equivalent to 
  ///
  ///     self.map(transform).flatten
  ///
  /// - Complexity: O(1)
  public func flatMap<Intermediate: CollectionType>(
    transform: (Generator.Element)->Intermediate
  ) -> _prext_LazyCollection<
    _prext_FlattenCollection<
      _prext_LazyMapCollection<Elements, Intermediate>>
  > {
    return self.map(transform)._prext_flatten
  }
}

extension _prext_LazyCollectionType
where Generator.Element == Elements.Generator.Element,
Elements.Index : BidirectionalIndexType
{
  /// Returns the concatenated results of mapping `transform` over
  /// `self`.  Equivalent to 
  ///
  ///     self.map(transform).flatten
  ///
  /// - Complexity: O(1)
  public func flatMap<
    Intermediate: CollectionType
    where Intermediate.Index : BidirectionalIndexType
  >(
    transform: (Generator.Element)->Intermediate
  ) -> _prext_LazyCollection<
    _prext_FlattenBidirectionalCollection<
      _prext_LazyMapCollection<Elements, Intermediate>
  >> {
    return self.map(transform)._prext_flatten
  }
}
