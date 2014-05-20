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

protocol _ExtensibleCollection : Collection {
  init()

  /// A non-binding request to ensure `n` elements of available storage.
  /// This works as an optimization to avoid multiple reallocations of
  /// linear data structures like Array
  mutating func reserveCapacity(n: IndexType.DistanceType)

  /*
  The 'extend' requirement should be an operator, but the compiler crashes:

  <rdar://problem/16566712> Dependent type should have been substituted by Sema
  or SILGen

  @assignment func +=<
    S : Sequence
    where S.GeneratorType.Element == Self.GeneratorType.Element
  >(inout _: Self, _: S)
  */

  mutating func extend<
      S : Sequence
      where S.GeneratorType.Element == Self.GeneratorType.Element
  >(_: S)
}

protocol ExtensibleCollection : _ExtensibleCollection {
/*
  We could have these operators with default implementations, but the compiler
  crashes:

  <rdar://problem/16566712> Dependent type should have been substituted by Sema
  or SILGen

  func +<
    S : Sequence
    where S.GeneratorType.Element == Self.GeneratorType.Element
  >(_: Self, _: S) -> Self

  func +<
    S : Sequence
    where S.GeneratorType.Element == Self.GeneratorType.Element
  >(_: S, _: Self) -> Self

  func +<
    S : Collection
    where S.GeneratorType.Element == Self.GeneratorType.Element
  >(_: Self, _: S) -> Self
*/
}

func +<
    C : _ExtensibleCollection,
    S : Sequence
    where S.GeneratorType.Element == C.GeneratorType.Element
>(var lhs: C, rhs: S) -> C {
  // FIXME: what if lhs is a reference type?  This will mutate it.
  lhs.extend(rhs)
  return lhs
}

func +<
    C : _ExtensibleCollection,
    S : Sequence
    where S.GeneratorType.Element == C.GeneratorType.Element
>(lhs: S, rhs: C) -> C {
  var result = C()
  result.reserveCapacity(
    countElements(rhs) + numericCast(underestimateCount(lhs)))
  result.extend(lhs)
  result.extend(rhs)
  return result
}

func +<
    C : _ExtensibleCollection,
    S : Collection
    where S.GeneratorType.Element == C.GeneratorType.Element
>(var lhs: C, rhs: S) -> C {
  // FIXME: what if lhs is a reference type?  This will mutate it.
  lhs.reserveCapacity(countElements(lhs) + numericCast(countElements(rhs)))
  lhs.extend(rhs)
  return lhs
}

extension String : ExtensibleCollection {
  mutating func reserveCapacity(n: Int) {
    // FIXME: implement.
    // <rdar://problem/16970908> Implement String.reserveCapacity
  }

  mutating func extend<
      S : Sequence
      where S.GeneratorType.Element == Character
  >(seq: S) {
    for c in seq {
      self += c
    }
  }
}

/// Creates and returns a collection of type `C` that is the result of
/// interposing a given separator between the elements of the sequence
/// `elements`.
///
/// For example, this code excerpt writes "``here be dragons``" to the standard
/// output::
///
///   println(join(" ", [ "here", "be", "dragons" ]))
func join<
    C : ExtensibleCollection, S : Sequence where S.GeneratorType.Element == C
>(
  separator: C, elements: S
) -> C {
  var result = C()
  let separatorSize = countElements(separator)

  // FIXME: include separator
  let reservation = elements~>_preprocessingPass {
    reduce($0, 0, { $0 + separatorSize + countElements($1) }) - separatorSize
  }

  if let n = reservation {
    result.reserveCapacity(n)
  }

  var needSeparator = false
  for x in elements {
    if needSeparator {
      result.extend(separator)
    }
    result.extend(x)
    needSeparator = true
  }

  return result
}

