/*
 This source file is part of the Swift.org open source project

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
 See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
*/

// MARK: - Parsed Windows root

@available(SwiftStdlib 9999, *)
internal struct _ParsedWindowsRoot {
  var rootEnd: _SystemString.Index
  var relativeBegin: _SystemString.Index
  var drive: FilePath.CodeUnit?
  var deviceSigil: FilePath.CodeUnit?
}

@available(SwiftStdlib 9999, *)
extension _ParsedWindowsRoot {
  static func traditional(
    drive: FilePath.CodeUnit?,
    endingAt idx: _SystemString.Index
  ) -> _ParsedWindowsRoot {
    _ParsedWindowsRoot(
      rootEnd: idx,
      relativeBegin: idx,
      drive: drive,
      deviceSigil: nil)
  }

  static func unc(
    deviceSigil: FilePath.CodeUnit?,
    endingAt end: _SystemString.Index,
    relativeBegin relBegin: _SystemString.Index
  ) -> _ParsedWindowsRoot {
    _ParsedWindowsRoot(
      rootEnd: end,
      relativeBegin: relBegin,
      drive: nil,
      deviceSigil: deviceSigil)
  }

  static func device(
    deviceSigil: FilePath.CodeUnit,
    drive: FilePath.CodeUnit?,
    endingAt end: _SystemString.Index,
    relativeBegin relBegin: _SystemString.Index
  ) -> _ParsedWindowsRoot {
    _ParsedWindowsRoot(
      rootEnd: end,
      relativeBegin: relBegin,
      drive: drive,
      deviceSigil: deviceSigil)
  }

  var isVerbatimComponent: Bool {
    deviceSigil == ._question
  }
}

// MARK: - Lexer

@available(SwiftStdlib 9999, *)
struct _Lexer {
  var slice: Slice<_SystemString>

  init(_ str: _SystemString) {
    self.slice = str[...]
  }

  var backslash: FilePath.CodeUnit { ._backslash }

  mutating func eatBackslash() -> Bool {
    slice._eat(._backslash) != nil
  }

  // A drive letter is any single non-separator code unit immediately
  // followed by a colon. This matches Windows' own
  // RtlDetermineDosPathNameType_U, which keys on the second character
  // being a colon and does not validate the first: `1:`, `::`, etc. are
  // all drives. (A colon is itself a non-separator, so `::foo` parses as
  // drive `:` — the documented basis for the `=::` environment variable
  // Windows creates.)
  //
  // The non-separator requirement is what keeps leading-separator paths
  // out: `\:x` (and `/:x`, which `_normalizeSeparators` has already
  // rewritten to `\:x` by this point) are classified as rooted/UNC, not
  // drives. Because `/`→`\` conversion has already run, `isSeparator`
  // — which tests `\` only — is exactly the right predicate here.
  mutating func eatDrive() -> FilePath.CodeUnit? {
    let copy = slice
    if let d = slice._eat(if: { !_isSeparator($0) }),
       slice._eat(._colon) != nil {
      return d
    }
    slice = copy
    return nil
  }

  mutating func eatSigil() -> FilePath.CodeUnit? {
    let copy = slice
    guard let sigil = slice._eat(._question) ?? slice._eat(._dot) else {
      return nil
    }
    guard isEmpty || slice.first == backslash else {
      slice = copy
      return nil
    }
    return sigil
  }

  mutating func eatUNC() -> Bool {
    slice._eatSequence("UNC"._asciiBytes) != nil
  }

  mutating func eatComponent() -> Range<_SystemString.Index> {
    let backslash = self.backslash
    let component = slice._eatWhile({ $0 != backslash })
      ?? slice[slice.startIndex ..< slice.startIndex]
    return component.indices
  }

  var isEmpty: Bool {
    return slice.isEmpty
  }

  var current: _SystemString.Index { slice.startIndex }

  mutating func clear() {
    self = _Lexer(_SystemString())
  }

  mutating func reset(to str: _SystemString, at idx: _SystemString.Index) {
    self.slice = str[idx...]
  }
}

// MARK: - Verbatim prefix detection (pre-normalization)

@available(SwiftStdlib 9999, *)
extension _SystemString {
  // Check if this string starts with the exact verbatim prefix \\?\
  // (four backslashes — no forward slashes). Returns the index past
  // the prefix, or nil.
  internal func _startsWithVerbatimPrefix() -> Index? {
    var s = self[...]
    guard s._eatSequence(#"\\?\"#._asciiBytes) != nil else { return nil }
    return s.startIndex
  }

  // For a verbatim path (exact \\?\ prefix), find where the anchor
  // ends. Only backslash is a separator in verbatim context.
  // Returns the index where component content begins.
  internal func _findVerbatimAnchorEnd() -> Index {
    guard let afterPrefix = _startsWithVerbatimPrefix() else {
      return startIndex
    }

    func skipToSep(from start: Index) -> Index {
      var i = start
      while i < endIndex && !_isSeparator(self[i]) {
        formIndex(after: &i)
      }
      return i
    }

    func skipPastSep(from idx: Index) -> Index {
      idx < endIndex && _isSeparator(self[idx]) ? index(after: idx) : idx
    }

    // \\?\UNC\server\share[\]
    var s = self[afterPrefix...]
    if s._eatSequence("UNC"._asciiBytes) != nil,
       s._eat(._backslash) != nil {
      let serverEnd = skipToSep(from: s.startIndex)
      let shareStart = skipPastSep(from: serverEnd)
      let shareEnd = skipToSep(from: shareStart)
      return skipPastSep(from: shareEnd)
    }

    // \\?\<drive>:[\] — a drive letter is any single non-separator code
    // unit before the colon (matching eatDrive). In verbatim paths
    // separators are not normalized and `/` is a legal component byte,
    // so `!isSeparator` accepts it: `\\?\/:` parses with drive `/`,
    // taking the bytes as written.
    s = self[afterPrefix...]
    if s._eat(if: { !_isSeparator($0) }) != nil,
       s._eat(._colon) != nil {
      return skipPastSep(from: s.startIndex)
    }

    // \\?\device[\]
    let deviceEnd = skipToSep(from: afterPrefix)
    return skipPastSep(from: deviceEnd)
  }
}

// MARK: - Windows root parsing

@available(SwiftStdlib 9999, *)
extension _SystemString {
  internal func _parseWindowsRootInternal() -> _ParsedWindowsRoot? {
    _internalInvariant(_isWindows)

    var lexer = _Lexer(self)

    func parseUNC(
      deviceSigil: FilePath.CodeUnit?
    ) -> _ParsedWindowsRoot {
      _ = lexer.eatComponent()
      guard lexer.eatBackslash() else {
        let end = lexer.current
        return .unc(
          deviceSigil: deviceSigil,
          endingAt: end,
          relativeBegin: end)
      }
      _ = lexer.eatComponent()
      let rootEnd = lexer.current
      _ = lexer.eatBackslash()
      return .unc(
        deviceSigil: deviceSigil,
        endingAt: rootEnd, relativeBegin: lexer.current)
    }

    // `C:` or `C:\`
    if let d = lexer.eatDrive() {
      _ = lexer.eatBackslash()
      return .traditional(
        drive: d,
        endingAt: lexer.current)
    }

    guard lexer.eatBackslash() else { return nil }
    guard lexer.eatBackslash() else {
      return .traditional(
        drive: nil,
        endingAt: lexer.current)
    }

    guard let sigil = lexer.eatSigil() else {
      return parseUNC(deviceSigil: nil)
    }

    guard lexer.eatBackslash() else {
      return .device(
        deviceSigil: sigil,
        drive: nil,
        endingAt: lexer.current,
        relativeBegin: lexer.current)
    }

    // UNC sub-form only applies to verbatim paths (\\?\UNC\...).
    // For device-namespace (\\.\), UNC is just a device name.
    if sigil == ._question, lexer.eatUNC() {
      guard lexer.eatBackslash() else {
        let end = lexer.current
        return .device(
          deviceSigil: sigil,
          drive: nil,
          endingAt: end,
          relativeBegin: end)
      }
      return parseUNC(deviceSigil: sigil)
    }

    // Check for device drive: \\.\C:\ or \\?\C:\
    let deviceRange = lexer.eatComponent()
    let rootEnd = lexer.current

    // A drive letter is any single non-separator code unit before the
    // colon (matching `eatDrive`). In verbatim paths `/` is a non-separator
    // and legal, so `\\?\/:` parses with drive `/`.
    let drive = _driveLetter(of: self[deviceRange])
    if drive != nil, lexer.eatBackslash() {
      // \\?\C:\  or \\.\C:\
      let newEnd = lexer.current
      return .device(
        deviceSigil: sigil,
        drive: drive,
        endingAt: newEnd,
        relativeBegin: newEnd)
    }

    _ = lexer.eatBackslash()

    return .device(
      deviceSigil: sigil,
      drive: drive,
      endingAt: rootEnd, relativeBegin: lexer.current)
  }

  internal func _parseWindowsRoot() -> (
    rootEnd: _SystemString.Index,
    relativeBegin: _SystemString.Index
  ) {
    guard let parsed = _parseWindowsRootInternal() else {
      return (startIndex, startIndex)
    }
    return (parsed.rootEnd, parsed.relativeBegin)
  }
}

// Returns the drive letter if `slice` is exactly the 2-byte drive form
// `<non-separator><colon>`, else nil. Intended for testing already-
// extracted device slices; for parsing-from-stream, see `_Lexer.eatDrive`.
@available(SwiftStdlib 9999, *)
private func _driveLetter(
  of slice: Slice<_SystemString>
) -> FilePath.CodeUnit? {
  var s = slice
  guard let first = s._eat(if: { !_isSeparator($0) }),
        s._eat(._colon) != nil,
        s.isEmpty
  else { return nil }
  return first
}

// MARK: - Windows root prenormalization

@available(SwiftStdlib 9999, *)
extension _SystemString {
  internal mutating func _prenormalizeWindowsRoots() -> Index {
    _internalInvariant(_isWindows)

    var lexer = _Lexer(self)

    guard lexer.eatBackslash(), lexer.eatBackslash() else {
      return lexer.current
    }

    // Three or more leading backslashes: NOT a UNC/device path.
    // Return after the first backslash; coalescing handles the rest.
    if !lexer.isEmpty && lexer.slice.first == ._backslash {
      return self.index(after: self.startIndex)
    }

    func expectBackslash() {
      if lexer.eatBackslash() { return }
      let idx = lexer.current
      lexer.clear()
      self.insert(._backslash, at: idx)
      lexer.reset(to: self, at: idx)
      let p = lexer.eatBackslash()
      _internalInvariant(p)
    }
    func expectComponent() {
      _ = lexer.eatComponent()
      expectBackslash()
    }

    if let sigil = lexer.eatSigil() {
      expectBackslash()
      // UNC sub-form only for verbatim (\\?\UNC\...), not device (\\.\UNC\...)
      if sigil == ._question, lexer.eatUNC() {
        expectBackslash()
        expectComponent()          // server: its separator is structural
        // Share: consume a trailing separator only if one is actually
        // present — never synthesize one. The share can be the final
        // element of a verbatim-UNC root with no trailing separator
        // (\\?\UNC\s\h); forcing a backslash here would store a phantom
        // trailing separator and make it compare equal to \\?\UNC\s\h\.
        // Mirrors parseUNC's `_ = lexer.eatBackslash()` and the
        // `!lexer.isEmpty`-guarded device path.
        _ = lexer.eatComponent()
        _ = lexer.eatBackslash()
        return lexer.current
      }
      // Check for drive letter device: \\.\C:\ or \\?\C:\. A drive
      // letter is any single non-separator code unit before the colon.
      let deviceRange = lexer.eatComponent()
      if _driveLetter(of: self[deviceRange]) != nil {
        // Eat the trailing backslash if present (both branches return
        // `lexer.current`).
        _ = lexer.eatBackslash()
        return lexer.current
      }
      // Only expect trailing backslash if there's more content
      if !deviceRange.isEmpty && !lexer.isEmpty {
        expectBackslash()
      }
      return lexer.current
    }

    expectComponent()
    return lexer.current
  }
}
