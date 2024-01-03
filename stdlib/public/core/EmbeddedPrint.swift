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

import SwiftShims

// TODO: This is all a stop-gap so that at least some types are printable in
// embedded Swift, in an embedded-programming friendly way (we mainly need
// printing to not need to heap allocate).

@_silgen_name("putchar")
func putchar(_: UInt8)

public func print(_ string: StaticString, terminator: StaticString = "\n") {
  var p = string.utf8Start
  while p.pointee != 0 {
    putchar(p.pointee)
    p += 1
  }
  p = terminator.utf8Start
  while p.pointee != 0 {
    putchar(p.pointee)
    p += 1
  }
}

func printCharacters(_ buf: UnsafeRawBufferPointer) {
  for c in buf {
    putchar(c)
  }
}

func printCharacters(_ buf: UnsafeBufferPointer<UInt8>) {
  printCharacters(UnsafeRawBufferPointer(buf))
}

extension BinaryInteger {
  func writeToStdout(radix: Int = 10) {
    if self == (0 as Self) {
      print("0", terminator: "")
      return
    }
    
    func _ascii(_ digit: UInt8) -> UInt8 {
      if digit < 10 {
        UInt8(("0" as Unicode.Scalar).value) + digit
      } else {
        UInt8(("a" as Unicode.Scalar).value) + (digit - 10)
      }
    }
    let isNegative = Self.isSigned && self < (0 as Self)
    var value = magnitude
    withUnsafeTemporaryAllocation(byteCount: 64, alignment: 1) { buffer in
      var index = buffer.count - 1
      while value != 0 {
        let (quotient, remainder) =
            value.quotientAndRemainder(dividingBy: Magnitude(radix))
        buffer[index] = _ascii(UInt8(truncatingIfNeeded: remainder))
        index -= 1
        value = quotient
      }
      if isNegative {
        buffer[index] = UInt8(("-" as Unicode.Scalar).value)
        index -= 1
      }
      let start = index + 1
      let end = buffer.count - 1
      let count = end - start + 1

      let pointerToPrint = buffer.baseAddress?.advanced(by: start).assumingMemoryBound(to: UInt8.self)
      printCharacters(UnsafeBufferPointer(start: pointerToPrint, count: count))
    }
  }
}

public func print(_ integer: some BinaryInteger, terminator: StaticString = "\n") {
  integer.writeToStdout()
  print("", terminator: terminator)
}

public func print(_ boolean: Bool, terminator: StaticString = "\n") {
  if boolean {
    print("true", terminator: terminator)
  } else {
    print("false", terminator: terminator)
  }
}
