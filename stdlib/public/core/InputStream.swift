//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

#if SWIFT_STDLIB_HAS_STDIN

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
  var utf8Start: UnsafeMutablePointer<UInt8>?
  let utf8Count = unsafe swift_stdlib_readLine_stdin(&utf8Start)
  defer {
    unsafe _swift_stdlib_free(utf8Start)
  }
  guard utf8Count > 0 else {
    return nil
  }
  let utf8Buffer = unsafe UnsafeBufferPointer(start: utf8Start, count: utf8Count)
  var result = unsafe String._fromUTF8Repairing(utf8Buffer).result
  if strippingNewline, result.last == "\n" || result.last == "\r\n" {
    _ = result.removeLast()
  }
  return result
}

#endif
