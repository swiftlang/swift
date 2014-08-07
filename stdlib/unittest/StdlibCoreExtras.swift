//===--- StdlibCoreExtras.swift -------------------------------------------===//
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

//
// These APIs don't really belong in a unit testing library, but they are
// useful in tests, and stdlib does not have such facilities yet.
//

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
func scan<
  S : SequenceType, U
>(seq: S, initial: U, combine: (U, S.Generator.Element) -> U) -> [U] {
  var result = [U]()
  result.reserveCapacity(underestimateCount(seq))
  var runningResult = initial
  for element in seq {
    runningResult = combine(runningResult, element)
    result.append(runningResult)
  }
  return result
}

func findSubstring<
  C1 : Sliceable, C2 : Sliceable
where
  C1.Generator.Element == C1.SubSlice.Generator.Element,
  C1.SubSlice : Sliceable,
  C1.SubSlice == C1.SubSlice.SubSlice,
  C1.Generator.Element == C2.Generator.Element,
  C1.Generator.Element : Equatable
>(string: C1, substring: C2) -> C1.Index? {
  var currentString = string[string.startIndex..<string.endIndex]
  for i in string.startIndex..<string.endIndex {
    if startsWith(currentString, substring) {
      return i
    }
    currentString = dropFirst(currentString)
  }
  return nil
}

func withArrayOfCStrings<R>(
  args: [String], body: (Array<UnsafeMutablePointer<CChar>>) -> R
) -> R {

  let argsLengths = Array(map(args) { countElements($0.utf8) + 1 })
  let argsOffsets = [ 0 ] + scan(argsLengths, 0, +)
  let argsBufferSize = argsOffsets.last!

  var argsBuffer = [UInt8]()
  argsBuffer.reserveCapacity(argsBufferSize)
  for arg in args {
    argsBuffer += arg.utf8
    argsBuffer += [ 0 ]
  }

  return argsBuffer.withUnsafeBufferPointer {
    (argsBuffer) in
    let ptr = UnsafeMutablePointer<CChar>(argsBuffer.baseAddress)
    var cStrings = Array(map(argsOffsets) { ptr + $0 })
    cStrings.append(nil)
    return body(cStrings)
  }
}

