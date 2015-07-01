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

/// Creates and returns a collection of type `C` that is the result of
/// interposing a given separator between the elements of the sequence
/// `elements`.
///
/// For example, this code excerpt writes "``here be dragons``" to the standard
/// output:
///
///     print(join(" ", [ "here", "be", "dragons" ]))
public func join<
  C : RangeReplaceableCollectionType, S : SequenceType
  where S.Generator.Element == C
>(
  separator: C, _ elements: S
) -> C {
  var result = C()
  let separatorSize = separator.count

  // FIXME: include separator
  let reservation = elements._preprocessingPass {
    (s: S) -> C.Index.Distance in
    let r: C.Index.Distance = s.reduce(0) { $0 + separatorSize + $1.count }
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

