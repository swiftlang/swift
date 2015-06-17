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

/// A collection type that can be efficiently appended-to.
public protocol ExtensibleCollectionType : CollectionType {
  /// Create an empty instance.
  init()

  /// A non-binding request to ensure `n` elements of available storage.
  ///
  /// This works as an optimization to avoid multiple reallocations of
  /// linear data structures like `Array`.  Conforming types may
  /// reserve more than `n`, exactly `n`, less than `n` elements of
  /// storage, or even ignore the request completely.
  mutating func reserveCapacity(n: Index.Distance)

  /*
  The 'extend' requirement should be an operator, but the compiler crashes:

  <rdar://problem/16566712> Dependent type should have been substituted by Sema
  or SILGen

  func +=<
    S : SequenceType
    where S.Generator.Element == Generator.Element
  >(inout _: Self, _: S)
  */

  /// Append `x` to `self`.
  ///
  /// Applying `successor()` to the index of the new element yields
  /// `self.endIndex`.
  ///
  /// - Complexity: Amortized O(1).
  mutating func append(x: Generator.Element)

  /// Append the elements of `newElements` to `self`.
  ///
  /// - Complexity: O(*length of result*).
  ///
  /// A possible implementation:
  ///
  ///     reserveCapacity(self.count + newElements.underestimateCount())
  ///     for x in newElements {
  ///       self.append(x)
  ///     }
  mutating func extend<
      S : SequenceType
      where S.Generator.Element == Generator.Element
  >(newElements: S)

  /*
  We could have these operators with default implementations, but the compiler
  crashes:

  <rdar://problem/16566712> Dependent type should have been substituted by Sema
  or SILGen

  func +<
    S : SequenceType
    where S.Generator.Element == Generator.Element
  >(_: Self, _: S) -> Self

  func +<
    S : SequenceType
    where S.Generator.Element == Generator.Element
  >(_: S, _: Self) -> Self

  func +<
    S : CollectionType
    where S.Generator.Element == Generator.Element
  >(_: Self, _: S) -> Self

  func +<
    EC : ExtensibleCollectionType
    where EC.Generator.Element == Generator.Element
  >(_: Self, _: S) -> Self
*/
}

public func +<
    C : ExtensibleCollectionType,
    S : SequenceType
    where S.Generator.Element == C.Generator.Element
>(var lhs: C, rhs: S) -> C {
  // FIXME: what if lhs is a reference type?  This will mutate it.
  lhs.extend(rhs)
  return lhs
}

public func +<
    C : ExtensibleCollectionType,
    S : SequenceType
    where S.Generator.Element == C.Generator.Element
>(lhs: S, rhs: C) -> C {
  var result = C()
  result.reserveCapacity(rhs.count + numericCast(rhs.underestimateCount()))
  result.extend(lhs)
  result.extend(rhs)
  return result
}

public func +<
    C : ExtensibleCollectionType,
    S : CollectionType
    where S.Generator.Element == C.Generator.Element
>(var lhs: C, rhs: S) -> C {
  // FIXME: what if lhs is a reference type?  This will mutate it.
  lhs.reserveCapacity(lhs.count + numericCast(rhs.count))
  lhs.extend(rhs)
  return lhs
}

public func +<
    EC1 : ExtensibleCollectionType,
    EC2 : ExtensibleCollectionType
    where EC1.Generator.Element == EC2.Generator.Element
>(var lhs: EC1, rhs: EC2) -> EC1 {
  // FIXME: what if lhs is a reference type?  This will mutate it.
  lhs.reserveCapacity(lhs.count + numericCast(rhs.count))
  lhs.extend(rhs)
  return lhs
}

@available(*, unavailable, message="call the 'join' method on the first argument")
public func join<
 C : ExtensibleCollectionType, S : SequenceType 
 where S.Generator.Element : SequenceType,
       S.Generator.Element.Generator.Element == C.Generator.Element
> (
 separator: C, _ elements: S
) -> C {
  fatalError("unavailable")
}

extension ExtensibleCollectionType {
  /// Creates and returns a collection of type `Self` that is the
  /// result of concatenating `elements` with interposed copies of
  /// `self`.
  ///
  /// For example:
  ///
  ///     [1, 0].join([[5, 6], [7], [8, 9]]) // [5,6,  1,0,  7,  1,0,  8,9]
  ///
  /// - seealso: `String.join`
  public func join<
  S : SequenceType where S.Generator.Element : SequenceType,
    S.Generator.Element.Generator.Element == Generator.Element
  > (elements: S) -> Self {
    var result = Self()
    
    typealias Distance = Index.Distance
    let separatorCount = self.count

    let reservation = elements._preprocessingPass { s in
      s.reduce(0) {
        $0 + separatorCount + numericCast($1.underestimateCount())
      } - separatorCount
    }

    if let n = reservation {
      result.reserveCapacity(n)
    }

    if separatorCount != 0 {
      var gen = elements.generate()
      if let first = gen.next() {
        result.extend(first)
        while let next = gen.next() {
          result.extend(self)
          result.extend(next)
        }
      }
    } else {
      for x in elements {
        result.extend(x)
      }
    }

    return result
  }
}

