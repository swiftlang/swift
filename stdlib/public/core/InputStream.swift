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
  let bytes: [UInt8] = _readLine()
  if bytes.isEmpty {
    return nil
  }
  var result: String = bytes.withUnsafeBufferPointer {
    String._fromUTF8Repairing($0).result
  }
  if strippingNewline, result.last!.isNewline {
    _ = result.removeLast()
  }
  return result
}

//===----------------------------------------------------------------------===//
//          | Abbrev |               Alias | Code Point |    UTF-8 |          //
//          | :----: | ------------------: | :--------: | -------: |          //
//          |   LF   |           LINE FEED |   U+000A   |       0A |          //
//          |   VT   | VERTICAL TABULATION |   U+000B   |       0B |          //
//          |   FF   |           FORM FEED |   U+000C   |       0C |          //
//          |   CR   |     CARRIAGE RETURN |   U+000D   |       0D |          //
//          |   NEL  |           NEXT LINE |   U+0085   |    C2 85 |          //
//          |   LS   |      LINE SEPARATOR |   U+2028   | E2 80 A8 |          //
//          |   PS   | PARAGRAPH SEPARATOR |   U+2029   | E2 80 A9 |          //
//===----------------------------------------------------------------------===//

private func _readLine() -> [UInt8] {
  _swift_stdlib_flockfile_stdin()
  defer {
    _swift_stdlib_funlockfile_stdin()
  }

  var bytes = [UInt8]()
  while true {

    // ISO C requires the `EOF` (end-of-file) macro to be a negative value.
    guard let byte = UInt8(exactly: _swift_stdlib_getc_unlocked_stdin()) else {
      return bytes
    }
    bytes.append(byte)

    // The "\r\n" (CRLF) sequence is handled by two iterations of the loop.
    // Other sequences are searched for in reverse UTF-8 code unit order.
    switch byte {
    case 0x0A, 0x0B, 0x0C:
      return bytes
    case 0x0D:
      if _swift_stdlib_ungetc_unlocked_stdin(
           _swift_stdlib_getc_unlocked_stdin()) != 0x0A {
        return bytes
      }
    case 0x85:
      if bytes.count >= 2,
         bytes[bytes.count - 2] == 0xC2 {
        return bytes
      }
    case 0xA8, 0xA9:
      if bytes.count >= 3,
         bytes[bytes.count - 2] == 0x80,
         bytes[bytes.count - 3] == 0xE2 {
        return bytes
      }
    default:
      continue
    }
  }
}
