//===----------------------------------------------------------------------===//
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

import Swift
import SwiftShims

/// Convert the given numeric value to a hexadecimal string.
  // FIXME(integers): support a more general BinaryInteger protocol
public func asHex<T : FixedWidthInteger>(_ x: T) -> String {
  return "0x" + String(x, radix: 16)
}

/// Convert the given sequence of numeric values to a string representing
/// their hexadecimal values.
  // FIXME(integers): support a more general BinaryInteger protocol
public func asHex<S : Sequence>(_ x: S) -> String
  where
  S.Element : FixedWidthInteger {
  return "[ " + x.lazy.map { asHex($0) }.joined(separator: ", ") + " ]"
}

/// Compute the prefix sum of `seq`.
public func scan<
  S : Sequence, U
>(_ seq: S, _ initial: U, _ combine: (U, S.Element) -> U) -> [U] {
  var result: [U] = []
  result.reserveCapacity(seq.underestimatedCount)
  var runningResult = initial
  for element in seq {
    runningResult = combine(runningResult, element)
    result.append(runningResult)
  }
  return result
}

public func gather<C : Collection, IndicesSequence : Sequence>(
  _ collection: C, _ indices: IndicesSequence
) -> [C.Element]
  where IndicesSequence.Element == C.Index {
  return Array(indices.map { collection[$0] })
}

public func scatter<T>(_ a: [T], _ idx: [Int]) -> [T] {
  var result = a
  for i in 0..<a.count {
    result[idx[i]] = a[i]
  }
  return result
}

public func withArrayOfCStrings<R>(
  _ args: [String], _ body: ([UnsafeMutablePointer<CChar>?]) -> R
) -> R {
  let argsCounts = Array(args.map { $0.utf8.count + 1 })
  let argsOffsets = [ 0 ] + scan(argsCounts, 0, +)
  let argsBufferSize = argsOffsets.last!

  var argsBuffer: [UInt8] = []
  argsBuffer.reserveCapacity(argsBufferSize)
  for arg in args {
    argsBuffer.append(contentsOf: arg.utf8)
    argsBuffer.append(0)
  }

  return argsBuffer.withUnsafeMutableBufferPointer {
    (argsBuffer) in
    let ptr = UnsafeMutableRawPointer(argsBuffer.baseAddress!).bindMemory(
      to: CChar.self, capacity: argsBuffer.count)
    var cStrings: [UnsafeMutablePointer<CChar>?] = argsOffsets.map { ptr + $0 }
    cStrings[cStrings.count - 1] = nil
    return body(cStrings)
  }
}
