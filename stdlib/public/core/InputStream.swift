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
  _swift_stdlib_flockfile_stdin()
  defer {
    _swift_stdlib_funlockfile_stdin()
  }

  var result = ""
  while let character = _readCharacter() {
    result.append(character)
    if character == "\r", _peekByte() == UInt8(ascii: "\n") {
      continue
    }
    if character.isNewline {
      break
    }
  }

  guard !result.isEmpty else {
    return nil
  }
  if strippingNewline, result.last!.isNewline {
    _ = result.removeLast()
  }
  return result
}

//===----------------------------------------------------------------------===//

private let _replacementCharacter: Character = "\u{FFFD}"

private func _readCharacter() -> Character? {
  var utf8Bytes = _FixedArray4<UInt8>()
  do {
    // Leading byte...
    guard let byte = _readByte() else {
      return nil
    }
    guard !_isContinuation(byte) else {
      return _replacementCharacter
    }
    utf8Bytes.append(byte)
  }

  let utf8Count = _utf8ScalarLength(utf8Bytes[0])
  switch utf8Count {
  case 1:
    _internalInvariant(_isASCII(utf8Bytes[0]))
  case 2, 3, 4:
    for _ in 1 ..< utf8Count {
      // Continuation bytes...
      guard let byte = _readByte() else {
        return _replacementCharacter
      }
      guard _isContinuation(byte) else {
        _ = _pushByte(byte)
        return _replacementCharacter
      }
      utf8Bytes.append(byte)
    }
  default:
    return _replacementCharacter
  }

  return utf8Bytes.withUnsafeBufferPointer {
    _internalInvariant($0.count == utf8Count)
    let result = String._fromUTF8Repairing($0).result
    return result.count == 1 ? Character(result) : _replacementCharacter
  }
}

//===----------------------------------------------------------------------===//

@inline(__always)
private func _readByte() -> UInt8? {
  return UInt8(exactly: _swift_stdlib_getc_unlocked_stdin())
}

@inline(__always)
private func _pushByte(_ byte: UInt8) -> Bool {
  return UInt8(exactly: _swift_stdlib_ungetc_unlocked_stdin(CInt(byte))) == byte
}

@inline(__always)
private func _peekByte() -> UInt8? {
  guard let byte = _readByte(), _pushByte(byte) else {
    return nil
  }
  return byte
}
