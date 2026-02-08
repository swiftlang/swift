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
#if hasFeature(CustomAvailability)
@available(Unicode)
#endif
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
  if strippingNewline {
    result.removeNewline()
  }
  return result
}

extension String {
  /// Remove the newline from the string
  fileprivate mutating func removeNewline() {
    var scalars = unicodeScalars[...]

    // If the last character is not "\n", there's nothing to do.
    let newline: UnicodeScalar = "\n"
    guard let lastNewline = scalars.last, lastNewline == newline else {
      return
    }

    // Drop the "\n".
    scalars = scalars.dropLast()

    // Now remove "\r" if it's also there.
    let carriageReturn: UnicodeScalar = "\r"
    if let lastCR = scalars.last, lastCR == carriageReturn {
      // Drop the "\rn".
      scalars = scalars.dropLast()
    }

    // Remove from the end of the adjusted scalars to the end of this string.
    _guts.remove(from: scalars.endIndex, to: endIndex)
  }
}

#endif
