/*
 This source file is part of the Swift.org open source project

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
 See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
*/

// MARK: - Darwin anchor parsing

// Darwin extends the basic Unix root `/` with:
// - Resolve flags: /.nofollow/, /.resolve/N/
// - Volume references: /.vol/FSID/FILEID

@available(SwiftStdlib 9999, *)
internal struct _ParsedDarwinAnchor {
  var anchorEnd: _SystemString.Index
  var relativeBegin: _SystemString.Index
}

@available(SwiftStdlib 9999, *)
extension _SystemString {
  // Try to parse a Darwin-specific anchor.
  // Returns nil if this is just a plain `/` root.
  internal func _parseDarwinAnchor() -> _ParsedDarwinAnchor? {
    _internalInvariant(_isDarwin)
    guard !isEmpty, self.first == ._slash else { return nil }

    let afterSlash = index(after: startIndex)
    guard afterSlash < endIndex, self[afterSlash] == ._dot else { return nil }

    // Anchor grammar: /(flag)?(vol)? — flag is .nofollow/ or .resolve/N/,
    // vol is .vol/FSID/FILEID(/)?, at least one of the two must be present.
    let flag = _parseNofollow(from: afterSlash) ?? _parseResolve(from: afterSlash)
    let volStart = flag?.relativeBegin ?? afterSlash
    return _parseVol(from: volStart) ?? flag
  }

  // MARK: - /.nofollow/

  private func _parseNofollow(from dotIdx: Index) -> _ParsedDarwinAnchor? {
    var s = self[dotIdx...]
    guard s._eatSequence(".nofollow/"._asciiBytes) != nil else { return nil }
    return _ParsedDarwinAnchor(
      anchorEnd: s.startIndex, relativeBegin: s.startIndex)
  }

  // MARK: - /.resolve/<value>/

  private func _parseResolve(from dotIdx: Index) -> _ParsedDarwinAnchor? {
    var s = self[dotIdx...]
    guard s._eatSequence(".resolve/"._asciiBytes) != nil,
          s._eatWhile({ $0 != ._slash }) != nil,
          s._eat(._slash) != nil else { return nil }
    return _ParsedDarwinAnchor(
      anchorEnd: s.startIndex, relativeBegin: s.startIndex)
  }

  // MARK: - /.vol/FSID/FILEID

  private func _parseVol(from dotIdx: Index) -> _ParsedDarwinAnchor? {
    guard let body = _parseVolBody(from: dotIdx) else { return nil }
    return _ParsedDarwinAnchor(
      anchorEnd: body.fileidRange.upperBound,
      relativeBegin: body.relativeBegin)
  }

  // Shared parser for the `.vol/FSID/FILEID[/]` body. Returns the
  // FILEID range and the relative-portion start (past the trailing
  // `/` if any).
  private func _parseVolBody(
    from dotIdx: Index
  ) -> (fileidRange: Range<Index>, relativeBegin: Index)? {
    var s = self[dotIdx...]
    guard s._eatSequence(".vol/"._asciiBytes) != nil,
          s._eatWhile({ $0 != ._slash }) != nil,
          s._eat(._slash) != nil else { return nil }
    let fileidStart = s.startIndex
    guard s._eatWhile({ $0 != ._slash }) != nil else { return nil }
    let fileidEnd = s.startIndex
    let relBegin = s._eat(._slash) != nil ? s.startIndex : fileidEnd
    return (fileidStart..<fileidEnd, relBegin)
  }
}

// MARK: - Darwin anchor canonicalization

@available(SwiftStdlib 9999, *)
extension _SystemString {
  // /.resolve/1/        -> /.nofollow/
  // /.vol/NNNN/2/       -> /.vol/NNNN/@/
  // /.nofollow/.vol/NNNN/2/   -> /.nofollow/.vol/NNNN/@/
  // /.resolve/1/.vol/NNNN/2/  -> /.nofollow/.vol/NNNN/@/
  // /.resolve/3/.vol/NNNN/2/  -> /.resolve/3/.vol/NNNN/@/
  //
  // Both replacements are independent and may both fire on a single
  // combined anchor, so we don't return between them.
  internal mutating func _canonicalizeDarwinAnchor() {
    guard _isDarwin else { return }

    // /.resolve/1/ (12 bytes) -> /.nofollow/ (11 bytes). Storage shrinks
    // by one byte; any trailing .vol portion shifts accordingly. Step 2
    // below re-finds positions on the post-replacement storage, so the
    // shift is not load-bearing for callers.
    let resolveOne = "/.resolve/1/"
    if self.starts(with: resolveOne._asciiBytes) {
      let prefixEnd = self.index(startIndex, offsetBy: resolveOne.utf8.count)
      self.replaceSubrange(
        startIndex..<prefixEnd, with: "/.nofollow/"._asciiBytes)
      _internalInvariant(self.starts(with: "/.nofollow/"._asciiBytes))
    }

    // .vol/FSID/FILEID -> .vol/FSID/@. The vol portion may be at the
    // start of storage (bare) or right after a leading nofollow/resolve
    // flag (combined anchor).
    guard let volDot = _findAnchorVolDotPosition(),
          let body = _parseVolBody(from: volDot)
    else { return }
    let fileidSlice = self[body.fileidRange]
    if fileidSlice.count == 1
       && fileidSlice.first == FilePath.CodeUnit(_ascii: "2") {
      self.replaceSubrange(body.fileidRange, with: CollectionOfOne(._at))
    }
  }

  // Returns the index of the leading `.` of `.vol/...` within the
  // anchor, if present. Three valid positions:
  // - at startIndex+1, when storage starts with `/.vol/`
  // - just past a leading `/.nofollow/`
  // - just past a leading `/.resolve/<value>/`
  // Returns nil if the anchor has no `.vol/...` portion.
  private func _findAnchorVolDotPosition() -> Index? {
    // Bare /.vol/ — the `.` is at startIndex+1.
    if self.starts(with: "/.vol/"._asciiBytes) {
      return self.index(after: startIndex)
    }
    // /.nofollow/.vol/ — the `.` of `.vol` is right after `/.nofollow/`.
    var s = self[...]
    if s._eatSequence("/.nofollow/"._asciiBytes) != nil,
       self[s.startIndex...].starts(with: ".vol"._asciiBytes) {
      return s.startIndex
    }
    // /.resolve/<value>/.vol/ — variable-length value.
    s = self[...]
    if s._eatSequence("/.resolve/"._asciiBytes) != nil,
       s._eatWhile({ $0 != ._slash }) != nil,
       s._eat(._slash) != nil,
       self[s.startIndex...].starts(with: ".vol"._asciiBytes) {
      return s.startIndex
    }
    return nil
  }
}

// MARK: - Resource fork detection

@available(SwiftStdlib 9999, *)
extension _SystemString {
  // The resource fork suffix is exactly "/..namedfork/rsrc" (17 bytes)
  internal static var _resourceForkSuffix: String { "/..namedfork/rsrc" }

  internal var _resourceForkSuffixStart: _SystemString.Index? {
    guard _isDarwin else { return nil }
    let suffix = Self._resourceForkSuffix
    let suffixCount = suffix.utf8.count
    guard self.count >= suffixCount else { return nil }
    let suffixStart = self.index(endIndex, offsetBy: -suffixCount)
    return self[suffixStart...].elementsEqual(suffix._asciiBytes)
      ? suffixStart : nil
  }

  internal func _hasResourceForkSuffix() -> Bool {
    _resourceForkSuffixStart != nil
  }
}
