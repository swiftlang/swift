//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Takes an unflattened array of scalar ranges and some Equatable property and
// attempts to merge ranges who share the same Equatable property. E.g:
//
//     0x0 ... 0xA  = .control
//     0xB ... 0xB  = .control
//     0xC ... 0x1F = .control
//
//    into:
//
//    0x0 ... 0x1F = .control
public func flatten<T: Equatable>(
  _ unflattened: [(ClosedRange<UInt32>, T)]
) -> [(ClosedRange<UInt32>, T)] {
  var result: [(ClosedRange<UInt32>, T)] = []

  for elt in unflattened.sorted(by: { $0.0.lowerBound < $1.0.lowerBound }) {
    guard !result.isEmpty, result.last!.1 == elt.1 else {
      result.append(elt)
      continue
    }

    if elt.0.lowerBound == result.last!.0.upperBound + 1 {
      result[result.count - 1].0 = result.last!.0.lowerBound ... elt.0.upperBound
    } else {
      result.append(elt)
    }
  }

  return result
}

// Takes an unflattened array of scalars and some Equatable property and
// attempts to merge scalars into ranges who share the same Equatable
// property. E.g:
//
//     0x9 = .control
//     0xA = .control
//     0xB = .control
//     0xC = .control
//
//    into:
//
//    0x9 ... 0xC = .control
public func flatten<T: Equatable>(
  _ unflattened: [(UInt32, T)]
) -> [(ClosedRange<UInt32>, T)] {
  var result: [(ClosedRange<UInt32>, T)] = []

  for elt in unflattened.sorted(by: { $0.0 < $1.0 }) {
    guard !result.isEmpty, result.last!.1 == elt.1 else {
      result.append((elt.0 ... elt.0, elt.1))
      continue
    }

    if elt.0 == result.last!.0.upperBound + 1 {
      result[result.count - 1].0 = result.last!.0.lowerBound ... elt.0
    } else {
      result.append((elt.0 ... elt.0, elt.1))
    }
  }

  return result
}
