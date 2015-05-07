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

/// This protocol is an implementation detail of `ExtensibleCollectionType`; do
/// not use it directly.
///
/// Its requirements are inherited by `ExtensibleCollectionType` and thus must
/// be satisfied by types conforming to that protocol.
public protocol _ExtensibleCollectionType : CollectionType {
  /// Create an empty instance
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
    where S.Generator.Element == Self.Generator.Element
  >(inout _: Self, _: S)
  */

  /// Append `x` to `self`.
  ///
  /// Applying `successor()` to the index of the new element yields
  /// `self.endIndex`.
  ///
  /// - complexity: amortized O(1).
  mutating func append(x: Self.Generator.Element)

  /// Append the elements of `newElements` to `self`.
  ///
  /// - complexity: O(*length of result*)
  ///
  /// A possible implementation:
  ///
  ///     reserveCapacity(count(self) + underestimateCount(newElements))
  ///     for x in newElements {
  ///       self.append(x)
  ///     }
  mutating func extend<
      S : SequenceType
      where S.Generator.Element == Self.Generator.Element
  >(newElements: S)
}

/// A collection type that can be efficiently appended-to.
public protocol ExtensibleCollectionType : _ExtensibleCollectionType {
/*
  We could have these operators with default implementations, but the compiler
  crashes:

  <rdar://problem/16566712> Dependent type should have been substituted by Sema
  or SILGen

  func +<
    S : SequenceType
    where S.Generator.Element == Self.Generator.Element
  >(_: Self, _: S) -> Self

  func +<
    S : SequenceType
    where S.Generator.Element == Self.Generator.Element
  >(_: S, _: Self) -> Self

  func +<
    S : CollectionType
    where S.Generator.Element == Self.Generator.Element
  >(_: Self, _: S) -> Self

  func +<
    EC : ExtensibleCollectionType
    where EC.Generator.Element == Self.Generator.Element
  >(_: Self, _: S) -> Self
*/
}

public func +<
    C : _ExtensibleCollectionType,
    S : SequenceType
    where S.Generator.Element == C.Generator.Element
>(var lhs: C, rhs: S) -> C {
  // FIXME: what if lhs is a reference type?  This will mutate it.
  lhs.extend(rhs)
  return lhs
}

public func +<
    C : _ExtensibleCollectionType,
    S : SequenceType
    where S.Generator.Element == C.Generator.Element
>(lhs: S, rhs: C) -> C {
  var result = C()
  result.reserveCapacity(count(rhs) + numericCast(rhs.underestimateCount()))
  result.extend(lhs)
  result.extend(rhs)
  return result
}

public func +<
    C : _ExtensibleCollectionType,
    S : CollectionType
    where S.Generator.Element == C.Generator.Element
>(var lhs: C, rhs: S) -> C {
  // FIXME: what if lhs is a reference type?  This will mutate it.
  lhs.reserveCapacity(count(lhs) + numericCast(count(rhs)))
  lhs.extend(rhs)
  return lhs
}

public func +<
    EC1 : _ExtensibleCollectionType,
    EC2 : _ExtensibleCollectionType
    where EC1.Generator.Element == EC2.Generator.Element
>(var lhs: EC1, rhs: EC2) -> EC1 {
  // FIXME: what if lhs is a reference type?  This will mutate it.
  lhs.reserveCapacity(count(lhs) + numericCast(count(rhs)))
  lhs.extend(rhs)
  return lhs
}

/// Creates and returns a collection of type `C` that is the result of
/// interposing a given separator between the elements of the sequence
/// `elements`.
///
/// For example, this code excerpt writes "``here be dragons``" to the standard
/// output:
///
///     println(join(" ", [ "here", "be", "dragons" ]))
public func join<
  C : ExtensibleCollectionType, S : SequenceType
  where S.Generator.Element == C
>(
  separator: C, _ elements: S
) -> C {
  var result = C()
  let separatorSize = count(separator)

  // FIXME: include separator
  let reservation = elements~>_preprocessingPass {
    (s: S) -> C.Index.Distance in
    var r: C.Index.Distance = s.reduce(0) { $0 + separatorSize + count($1) }
    return r - separatorSize
  }

  if let n = reservation {
    result.reserveCapacity(n)
  }

  if separatorSize != 0 {
    var gen = elements.generate()
    if let first = gen.next() {
      result.extend(first)
      while let next = gen.next() {
        result.extend(separator)
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

