//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

/// Convert the given numeric value to a hexadecimal string.
public func asHex<T : IntegerType>(x: T) -> String {
  return "0x" + String(x.toIntMax(), radix: 16)
}

/// Convert the given sequence of numeric values to a string representing
/// their hexadecimal values.
public func asHex<
  S: SequenceType
where
  S.Generator.Element : IntegerType
>(x: S) -> String {
  return "[ " + x.lazy.map { asHex($0) }.joinWithSeparator(", ") + " ]"
}

/// Compute the prefix sum of `seq`.
public func scan<
  S : SequenceType, U
>(seq: S, _ initial: U, _ combine: (U, S.Generator.Element) -> U) -> [U] {
  var result: [U] = []
  result.reserveCapacity(seq.underestimateCount())
  var runningResult = initial
  for element in seq {
    runningResult = combine(runningResult, element)
    result.append(runningResult)
  }
  return result
}

public func randomShuffle<T>(a: [T]) -> [T] {
  var result = a
  for i in (1..<a.count).reverse() {
    // FIXME: 32 bits are not enough in general case!
    let j = Int(rand32(exclusiveUpperBound: UInt32(i + 1)))
    if i != j {
      swap(&result[i], &result[j])
    }
  }
  return result
}

public func gather<
  C : CollectionType,
  IndicesSequence : SequenceType
  where
  IndicesSequence.Generator.Element == C.Index
>(
  collection: C, _ indices: IndicesSequence
) -> [C.Generator.Element] {
  return Array(indices.map { collection[$0] })
}

public func scatter<T>(a: [T], _ idx: [Int]) -> [T] {
  var result = a
  for i in 0..<a.count {
    result[idx[i]] = a[i]
  }
  return result
}

public func withArrayOfCStrings<R>(
  args: [String], _ body: ([UnsafeMutablePointer<CChar>]) -> R
) -> R {

  let argsLengths = Array(args.map { $0.utf8.count + 1 })
  let argsOffsets = [ 0 ] + scan(argsLengths, 0, +)
  let argsBufferSize = argsOffsets.last!

  var argsBuffer: [UInt8] = []
  argsBuffer.reserveCapacity(argsBufferSize)
  for arg in args {
    argsBuffer.appendContentsOf(arg.utf8)
    argsBuffer.append(0)
  }

  return argsBuffer.withUnsafeBufferPointer {
    (argsBuffer) in
    let ptr = UnsafeMutablePointer<CChar>(argsBuffer.baseAddress)
    var cStrings = argsOffsets.map { ptr + $0 }
    cStrings[cStrings.count - 1] = nil
    return body(cStrings)
  }
}

