//===----------------------------------------------------------------------===//
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

import SwiftShims

/// Convert the given numeric value to a hexidecimal string.
public func asHex<T : IntegerType>(x: T) -> String {
  return "0x" + String(x.toIntMax(), radix: 16)
}

/// Convert the given sequence of numeric values to a string representing
/// their hexidecimal values.
public func asHex<
  S: SequenceType
where
  S.Generator.Element : IntegerType
>(x: S) -> String {
  return "[ " + ", ".join(lazy(x).map { asHex($0) }) + " ]"
}

/// Compute the prefix sum of `seq`.
public func scan<
  S : SequenceType, U
>(seq: S, initial: U, combine: (U, S.Generator.Element) -> U) -> _UnitTestArray<U> {
  var result = _UnitTestArray<U>()
  result.reserveCapacity(underestimateCount(seq))
  var runningResult = initial
  for element in seq {
    runningResult = combine(runningResult, element)
    result.append(runningResult)
  }
  return result
}

public func randomShuffle<T>(a: _UnitTestArray<T>) -> _UnitTestArray<T> {
  var result = a
  for var i = a.count - 1; i != 0; --i {
    // FIXME: 32 bits are not enough in general case!
    let j = Int(rand32(exclusiveUpperBound: UInt32(i + 1)))
    swap(&result[i], &result[j])
  }
  return result
}

public func gather<T>(a: _UnitTestArray<T>, idx: _UnitTestArray<Int>) -> _UnitTestArray<T> {
  var result = _UnitTestArray<T>()
  result.reserveCapacity(a.count)
  for i in 0..<a.count {
    result.append(a[idx[i]])
  }
  return result
}

public func scatter<T>(a: _UnitTestArray<T>, idx: _UnitTestArray<Int>) -> _UnitTestArray<T> {
  var result = a
  for i in 0..<a.count {
    result[idx[i]] = a[i]
  }
  return result
}

public func withArrayOfCStrings<R>(
  args: _UnitTestArray<String>, body: (Array<UnsafeMutablePointer<CChar>>) -> R
) -> R {

  let argsLengths = _UnitTestArray(map(args) { count($0.utf8) + 1 })
  let argsOffsets = [ 0 ] + scan(argsLengths, 0, +)
  let argsBufferSize = argsOffsets.last!

  var argsBuffer = _UnitTestArray<UInt8>()
  argsBuffer.reserveCapacity(argsBufferSize)
  for arg in args {
    argsBuffer.extend(arg.utf8)
    argsBuffer.append(0)
  }

  return argsBuffer.withUnsafeBufferPointer {
    (argsBuffer) in
    let ptr = UnsafeMutablePointer<CChar>(argsBuffer.baseAddress)
    var cStrings = map(argsOffsets) { ptr + $0 }
    cStrings[cStrings.count - 1] = nil
    return body(cStrings)
  }
}

