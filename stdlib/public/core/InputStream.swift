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

/// Returns a string read from standard input through the end of the current
/// line or until EOF is reached.
///
/// Standard input is interpreted as `UTF-8`. Invalid bytes are replaced by
/// Unicode [replacement characters][rc].
///
/// [rc]:
/// https://unicode.org/glossary/#replacement_character
///
/// - Parameter strippingNewline: If `true`, newline characters and character
///   combinations are stripped from the result; otherwise, newline characters
///   or character combinations are preserved. The default is `true`.
/// - Returns: The string of characters read from standard input. If EOF has
///   already been reached when `readLine()` is called, the result is `nil`.
public func readLine(strippingNewline: Bool = true) -> String? {
  var linePtrVar: UnsafeMutablePointer<UInt8>?
  var readBytes = swift_stdlib_readLine_stdin(&linePtrVar)
  if readBytes == -1 {
    return nil
  }
  _sanityCheck(readBytes >= 0,
    "unexpected return value from swift_stdlib_readLine_stdin")
  if readBytes == 0 {
    return ""
  }

  let linePtr = linePtrVar!
  if strippingNewline {
    // FIXME: Unicode conformance.  To fix this, we need to reimplement the
    // code we call above to get a line, since it will only stop on LF.
    //
    // <rdar://problem/20013999> Recognize Unicode newlines in readLine()
    //
    // Recognize only LF and CR+LF combinations for now.
    let cr = UInt8(ascii: "\r")
    let lf = UInt8(ascii: "\n")
    if readBytes == 1 && linePtr[0] == lf {
      return ""
    }
    if readBytes >= 2 {
      switch (linePtr[readBytes - 2], linePtr[readBytes - 1]) {
      case (cr, lf):
        readBytes -= 2
        break
      case (_, lf):
        readBytes -= 1
        break
      default:
        ()
      }
    }
  }
  let result = String._fromUTF8Repairing(
    UnsafeBufferPointer(start: linePtr, count: readBytes)).0
  _swift_stdlib_free(linePtr)
  return result
}
