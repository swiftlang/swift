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

#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
@_extern(c, "_swift_writeToStandardOutput")
private func _swift_writeToStandardOutput(
  _ pointer: UnsafePointer<UInt8>?,
  _ count: Int
) -> CInt
#else
@_extern(c, "putchar")
func putchar(_: CInt) -> CInt
#endif


internal func writeChars(_ chars: UnsafeBufferPointer<UInt8>) {
#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
  _ = unsafe _swift_writeToStandardOutput(chars.baseAddress, chars.count)
#else
  for unsafe char in unsafe chars {
    _ = putchar(CInt(char))
  }
#endif
}

extension StaticString {
  fileprivate func writeToStandardOutput() {
    withUTF8Buffer {
      unsafe writeChars($0)
    }
  }
}

extension String {
  fileprivate mutating func writeToStandardOutput() {
    withUTF8 {
      unsafe writeChars($0)
    }
  }
}

@inline(never)
public func print(_ string: StaticString, terminator: StaticString = "\n") {
  string.writeToStandardOutput()
  terminator.writeToStandardOutput()
}

@_disfavoredOverload
@inline(never)
public func print(_ string: String, terminator: StaticString = "\n") {
  var string = string
  string.writeToStandardOutput()
  terminator.writeToStandardOutput()
}

@_disfavoredOverload
@inline(never)
public func print(_ object: some CustomStringConvertible, terminator: StaticString = "\n") {
  var string = object.description
  string.writeToStandardOutput()
  terminator.writeToStandardOutput()
}

@inline(never)
func print(_ buf: UnsafeBufferPointer<UInt8>, terminator: StaticString = "\n") {
  unsafe writeChars(buf)
  terminator.writeToStandardOutput()
}

extension BinaryInteger {
  internal func _toStringImpl(
    _ buffer: UnsafeMutablePointer<UTF8.CodeUnit>,
    _ bufferLength: UInt,
    _ radix: Int,
    _ uppercase: Bool
  ) -> Int {
    if self == (0 as Self) {
      unsafe buffer[0] = UInt8(("0" as Unicode.Scalar).value)
      return 1
    }
    
    func _ascii(_ digit: UInt8) -> UTF8.CodeUnit {
      if digit < 10 {
        UInt8(("0" as Unicode.Scalar).value) + digit
      } else {
        UInt8(("a" as Unicode.Scalar).value) + (digit - 10)
      }
    }
    let isNegative = Self.isSigned && self < (0 as Self)
    var value = magnitude
    
    var index = Int(bufferLength - 1)
    while value != 0 {
      let (quotient, remainder) = value.quotientAndRemainder(dividingBy: Magnitude(radix))
      unsafe buffer[index] = _ascii(UInt8(truncatingIfNeeded: remainder))
      index -= 1
      value = quotient
    }
    if isNegative {
      unsafe buffer[index] = UInt8(("-" as Unicode.Scalar).value)
      index -= 1
    }
    let start = index + 1
    let end = Int(bufferLength - 1)
    let count = end - start + 1
    
    let intermediate = unsafe UnsafeBufferPointer(start: buffer.advanced(by: start), count: count)
    let destination = unsafe UnsafeMutableRawBufferPointer(start: buffer, count: Int(bufferLength))
    unsafe destination.copyMemory(from: UnsafeRawBufferPointer(intermediate))
    
    return count
  }

  func writeToStdout(radix: Int = 10) {
    // Avoid withUnsafeTemporaryAllocation which is not typed-throws ready yet
    let byteCount = 64
    let stackBuffer = Builtin.stackAlloc(byteCount._builtinWordValue,
         1._builtinWordValue, 1._builtinWordValue)
    let buffer = unsafe UnsafeMutableRawBufferPointer(start: .init(stackBuffer),
        count: byteCount).baseAddress!.assumingMemoryBound(to: UInt8.self)

    let count = unsafe _toStringImpl(buffer, 64, radix, false)

    unsafe writeChars(UnsafeBufferPointer(start: buffer, count: count))

    Builtin.stackDealloc(stackBuffer)
  }
}

@inline(never)
public func print(_ integer: some BinaryInteger, terminator: StaticString = "\n") {
  integer.writeToStdout(radix: 10)
  print("", terminator: terminator)
}

internal func printAsHex(_ integer: some BinaryInteger, terminator: StaticString = "\n") {
  integer.writeToStdout(radix: 16)
  print("", terminator: terminator)
}

@inline(never)
public func print(_ boolean: Bool, terminator: StaticString = "\n") {
  if boolean {
    print("true", terminator: terminator)
  } else {
    print("false", terminator: terminator)
  }
}

/// Hook used to print a fatal error from C code, e.g., parts of the Swift
/// Concurrency runtime.
@c @used
public func _swift_fatalError(_ message: UnsafePointer<UInt8>) -> Never {
  if _isDebugAssertConfiguration() {
    print("fatal error", terminator: ": ")

    var count = 0
    var pointer = unsafe message
    while unsafe pointer.pointee != 0 {
      count += 1
      unsafe pointer += 1
    }

    unsafe writeChars(UnsafeBufferPointer(start: message, count: count))
  } else {
    Builtin.condfail_message(true._value, message._rawValue)
  }
  Builtin.unreachable()
}
