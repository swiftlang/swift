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
    guard !isEmpty else { return nil }
    guard self.first == ._slash else { return nil }

    let afterSlash = index(after: startIndex)
    guard afterSlash < endIndex else { return nil }

    // Must start with /.
    guard self[afterSlash] == ._dot else { return nil }

    // Per the proposal, an anchor may include resolve flags AND/OR a
    // volume identifier; resolve always precedes vol. So a leading
    // /.nofollow/ or /.resolve/N/ may extend with .vol/FSID/FILEID into
    // a single combined anchor.
    if _matchesNofollow(from: afterSlash) {
      guard let leading = _parseNofollow(from: afterSlash) else { return nil }
      return _maybeChainVol(after: leading)
    }
    if _matchesResolve(from: afterSlash) {
      guard let leading = _parseResolve(from: afterSlash) else { return nil }
      return _maybeChainVol(after: leading)
    }

    // Bare /.vol/FSID/FILEID
    if _matchesVol(from: afterSlash) {
      return _parseVol(from: afterSlash)
    }

    return nil
  }

  // If `.vol/FSID/FILEID` follows the leading flag's relativeBegin,
  // extend the anchor to include it. Otherwise return the leading
  // anchor unchanged.
  private func _maybeChainVol(
    after leading: _ParsedDarwinAnchor
  ) -> _ParsedDarwinAnchor {
    let next = leading.relativeBegin
    guard next < endIndex,
          self[next] == ._dot,
          _matchesVol(from: next),
          let chained = _parseVol(from: next)
    else {
      return leading
    }
    // Chaining must strictly extend the leading anchor.
    _internalInvariant(chained.anchorEnd >= leading.relativeBegin)
    _internalInvariant(chained.relativeBegin >= chained.anchorEnd)
    return chained
  }

  // TODO(post-PR): see if we can avoid extra storage for below

  // ASCII byte spellings of the Darwin magic-anchor tokens, stored once
  // rather than rebuilt on every call. Indexing arithmetic uses each
  // array's own `.count` (a code-unit count); `String.count` would be a
  // grapheme-cluster count — coincidentally equal for these ASCII tokens,
  // but the wrong unit for indexing byte storage.
  private static let _nofollowToken: [FilePath.CodeUnit] =
    ".nofollow".unicodeScalars.map { FilePath.CodeUnit(_ascii: $0) }
  private static let _resolveToken: [FilePath.CodeUnit] =
    ".resolve".unicodeScalars.map { FilePath.CodeUnit(_ascii: $0) }
  private static let _volToken: [FilePath.CodeUnit] =
    ".vol".unicodeScalars.map { FilePath.CodeUnit(_ascii: $0) }

  // MARK: - /.nofollow/

  private func _matchesNofollow(from dotIdx: Index) -> Bool {
    self[dotIdx...].starts(with: Self._nofollowToken)
  }

  private func _parseNofollow(from dotIdx: Index) -> _ParsedDarwinAnchor? {
    let nofollowEnd = index(dotIdx, offsetBy: Self._nofollowToken.count)
    guard nofollowEnd < endIndex else { return nil }
    guard self[nofollowEnd] == ._slash else { return nil }

    let afterSlash = index(after: nofollowEnd)
    return _ParsedDarwinAnchor(
      anchorEnd: afterSlash,
      relativeBegin: afterSlash)
  }

  // MARK: - /.resolve/N/

  private func _matchesResolve(from dotIdx: Index) -> Bool {
    self[dotIdx...].starts(with: Self._resolveToken)
  }

  private func _parseResolve(from dotIdx: Index) -> _ParsedDarwinAnchor? {
    let resolveEnd = index(dotIdx, offsetBy: Self._resolveToken.count)
    guard resolveEnd < endIndex else { return nil }
    guard self[resolveEnd] == ._slash else { return nil }

    // Read the flag value (everything between the two slashes)
    let flagStart = index(after: resolveEnd)
    guard flagStart < endIndex else { return nil }

    // Find the closing slash
    guard let flagEnd = self[flagStart...].firstIndex(of: ._slash) else {
      return nil
    }
    guard flagStart < flagEnd else { return nil }

    let afterSlash = index(after: flagEnd)
    return _ParsedDarwinAnchor(
      anchorEnd: afterSlash,
      relativeBegin: afterSlash)
  }

  // MARK: - /.vol/FSID/FILEID

  private func _matchesVol(from dotIdx: Index) -> Bool {
    self[dotIdx...].starts(with: Self._volToken)
  }

  private func _parseVol(from dotIdx: Index) -> _ParsedDarwinAnchor? {
    let volEnd = index(dotIdx, offsetBy: Self._volToken.count)
    guard volEnd < endIndex else { return nil }
    guard self[volEnd] == ._slash else { return nil }

    // Read FSID
    let fsidStart = index(after: volEnd)
    guard fsidStart < endIndex else { return nil }
    guard let fsidEnd = self[fsidStart...].firstIndex(of: ._slash) else {
      return nil
    }
    guard fsidStart < fsidEnd else { return nil }

    // Read FILEID
    let fileidStart = index(after: fsidEnd)
    guard fileidStart < endIndex else { return nil }

    // FILEID goes up to next slash or end
    let fileidEnd = self[fileidStart...].firstIndex(of: ._slash) ?? endIndex
    guard fileidStart < fileidEnd else { return nil }

    // anchorEnd is at fileidEnd (the anchor is /.vol/FSID/FILEID without trailing /)
    let relBegin: _SystemString.Index
    if fileidEnd < endIndex && self[fileidEnd] == ._slash {
      relBegin = index(after: fileidEnd)
    } else {
      relBegin = fileidEnd
    }

    return _ParsedDarwinAnchor(
      anchorEnd: fileidEnd,
      relativeBegin: relBegin)
  }
}

// MARK: - Darwin anchor canonicalization

@available(SwiftStdlib 9999, *)
extension _SystemString {
  // TODO(post-PR): see if we can avoid extra storage

  // Full anchor prefixes used by canonicalization, stored once.
  private static let _resolveOneAnchor: [FilePath.CodeUnit] =
    "/.resolve/1/".unicodeScalars.map { FilePath.CodeUnit(_ascii: $0) }
  private static let _nofollowAnchor: [FilePath.CodeUnit] =
    "/.nofollow/".unicodeScalars.map { FilePath.CodeUnit(_ascii: $0) }
  private static let _volAnchor: [FilePath.CodeUnit] =
    "/.vol/".unicodeScalars.map { FilePath.CodeUnit(_ascii: $0) }

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

    // /.resolve/1/ (12 bytes) -> /.nofollow/ (11 bytes). Storage shrinks by
    // one byte; any trailing .vol portion shifts accordingly. Step 2 below
    // re-finds positions on the post-replacement storage, so the shift is
    // not load-bearing for callers.
    if self.starts(with: Self._resolveOneAnchor) {
      let resolveEnd = self.index(
        startIndex, offsetBy: Self._resolveOneAnchor.count)
      self.replaceSubrange(startIndex..<resolveEnd, with: Self._nofollowAnchor)
      _internalInvariant(self.starts(with: Self._nofollowAnchor))
    }

    // .vol/FSID/FILEID -> .vol/FSID/@. The vol portion may be at the
    // start of storage (bare) or right after a leading nofollow/resolve
    // flag (combined anchor).
    guard let volDot = _findAnchorVolDotPosition() else { return }
    _internalInvariant(self[volDot] == ._dot)
    _internalInvariant(_matchesVol(from: volDot))

    let volEnd = self.index(volDot, offsetBy: Self._volToken.count)
    guard volEnd < endIndex, self[volEnd] == ._slash else { return }

    let fsidStart = self.index(after: volEnd)
    guard fsidStart < endIndex else { return }
    guard let fsidEnd = self[fsidStart...].firstIndex(of: ._slash) else {
      return
    }
    guard fsidStart < fsidEnd else { return }

    let fileidStart = self.index(after: fsidEnd)
    guard fileidStart < endIndex else { return }

    let fileidEnd = self[fileidStart...].firstIndex(of: ._slash) ?? endIndex

    let fileidSlice = self[fileidStart..<fileidEnd]
    if fileidSlice.count == 1
       && fileidSlice.first == FilePath.CodeUnit(_ascii: "2") {
      self.replaceSubrange(fileidStart..<fileidEnd, with: CollectionOfOne(._at))
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
    if self.starts(with: Self._volAnchor) {
      return self.index(after: startIndex)
    }
    // /.nofollow/.vol/ — the `.` of `.vol` is right after `/.nofollow/`.
    if self.starts(with: Self._nofollowAnchor) {
      let after = self.index(
        startIndex, offsetBy: Self._nofollowAnchor.count)
      if after < endIndex, self[after] == ._dot, _matchesVol(from: after) {
        return after
      }
      return nil
    }
    // /.resolve/<value>/.vol/ — match prefix manually since <value> is
    // variable length.
    guard !isEmpty, self.first == ._slash else { return nil }
    let afterFirstSlash = self.index(after: startIndex)
    guard self[afterFirstSlash...].starts(with: Self._resolveToken) else {
      return nil
    }
    let afterResolve = self.index(
      afterFirstSlash, offsetBy: Self._resolveToken.count)
    guard afterResolve < endIndex, self[afterResolve] == ._slash else {
      return nil
    }
    let valueStart = self.index(after: afterResolve)
    guard let valueEnd = self[valueStart...].firstIndex(of: ._slash) else {
      return nil
    }
    guard valueStart < valueEnd else { return nil }
    let after = self.index(after: valueEnd)
    if after < endIndex, self[after] == ._dot, _matchesVol(from: after) {
      return after
    }
    return nil
  }
}

// MARK: - Resource fork detection

@available(SwiftStdlib 9999, *)
extension _SystemString {
  // The resource fork suffix is exactly "/..namedfork/rsrc" (17 bytes)
  internal static let _resourceForkSuffix: [FilePath.CodeUnit] =
    "/..namedfork/rsrc".unicodeScalars.map { FilePath.CodeUnit(_ascii: $0) }

  internal func _hasResourceForkSuffix() -> Bool {
    guard _isDarwin else { return false }
    let suffix = _SystemString._resourceForkSuffix
    guard self.count >= suffix.count else { return false }

    let suffixStart = self.index(endIndex, offsetBy: -suffix.count)
    return self[suffixStart...].elementsEqual(suffix)
  }

  internal var _resourceForkSuffixStart: _SystemString.Index? {
    guard _hasResourceForkSuffix() else { return nil }
    let suffix = _SystemString._resourceForkSuffix
    return self.index(endIndex, offsetBy: -suffix.count)
  }
}
