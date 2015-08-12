//===--- Join.swift - Protocol and Algorithm for concatenation ------------===//
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

extension RangeReplaceableCollectionType {
  /// Creates and returns a collection of type `Self` that is the result of
  /// interposing a given separator `self` between the elements of the sequence
  /// `elements`.
  ///
  /// For example, `[-1, -2].join([[1, 2, 3], [4, 5, 6], [7, 8, 9]])`
  /// yields `[1, 2, 3, -1, -2, 4, 5, 6, -1, -2, 7, 8, 9]`.
  @warn_unused_result
  public func join<
    S : SequenceType where S.Generator.Element == Self
  >(elements: S) -> Self {
    var result = Self()
    let separatorSize = self.count

    let reservation = elements._preprocessingPass {
      (s: S) -> Index.Distance in
      let r: Index.Distance = s.reduce(0) { $0 + separatorSize + $1.count }
      return r - separatorSize
    }

    if let n = reservation {
      result.reserveCapacity(n)
    }

    if separatorSize != 0 {
      var gen = elements.generate()
      if let first = gen.next() {
        result.appendContentsOf(first)
        while let next = gen.next() {
          result.appendContentsOf(self)
          result.appendContentsOf(next)
        }
      }
    } else {
      for x in elements {
        result.appendContentsOf(x)
      }
    }

    return result
  }
}

@available(*, unavailable, message="call the 'join()' method on the collection")
public func join<
  C : RangeReplaceableCollectionType, S : SequenceType
  where S.Generator.Element == C
>(
  separator: C, _ elements: S
) -> C {
  fatalError("unavailable function can't be called")
}

