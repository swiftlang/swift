/*
 This source file is part of the Swift.org open source project

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
 See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
*/

// MARK: - Platform predicates
//
// Compile-time platform selection. This reference implementation builds for a
// single platform at a time, so these fold to constants.
#if os(Windows)
internal var _isWindows: Bool { true }
internal var _isDarwin:  Bool { false }
internal var _isLinux:   Bool { false }
#elseif os(anyAppleOS) || canImport(Darwin)
internal var _isWindows: Bool { false }
internal var _isDarwin:  Bool { true }
internal var _isLinux:   Bool { false }
// TODO(post-merge): what all can we fold in here? basically any generic POSIX platform that folds `//` into `/`
// ... #elseif os(Linux) || os(Android) || os(FreeBSD) || os(OpenBSD) || os(WASI)
#elseif os(Linux)
internal var _isWindows: Bool { false }
internal var _isDarwin:  Bool { false }
internal var _isLinux:   Bool { true }
#else
#error("FilePath: unsupported platform")
#endif

// The separator we use for slash-based platforms
@available(SwiftStdlib 9999, *)
private var _genericSeparator: FilePath.CodeUnit { ._slash }

@available(SwiftStdlib 9999, *)
internal var _platformSeparator: FilePath.CodeUnit {
  _isWindows ? ._backslash : _genericSeparator
}

@available(SwiftStdlib 9999, *)
internal func _isSeparator(_ c: FilePath.CodeUnit) -> Bool {
  c == _platformSeparator
}

// MARK: - Anchor shape classification

/// Returns `true` if the given anchor bytes are the Windows
/// drive-relative form `<letter>:` (e.g. `C:`).
///
/// Drive-relative is the *only* 2-byte anchor across all platforms:
/// Linux `/` is 1 byte; Darwin magic anchors (`/.nofollow/`,
/// `/.resolve/N/`, `/.vol/FSID/FILEID`) are all longer; UNC
/// (`\\server\share`) and verbatim variants are all longer. So the
/// "2-byte anchor ending in `:`" shape uniquely identifies
/// drive-relative — and traps if we're not on Windows.
///
/// The `:` IS the boundary in this anchor: `C:foo` is valid
/// (drive-relative with one component); `C:\foo` is a different
/// anchor (drive-absolute). Other anchor shapes that happen to end
/// in `:` — UNC with a colon-ending share name (`\\server\C:`),
/// Darwin volfs with a colon-ending FILEID — are NOT 2 bytes and
/// don't match.
@available(SwiftStdlib 9999, *)
internal func _isDriveRelativeAnchor(
  _ anchorBytes: some BidirectionalCollection<FilePath.CodeUnit>
) -> Bool {
  guard anchorBytes.count == 2, anchorBytes.last == ._colon else {
    return false
  }
  _internalInvariant(_isWindows, "2-byte colon anchor only exists on Windows")
  return true
}

/// Returns `true` if a separator must be inserted between the given
/// anchor bytes and the first component byte that follows them.
///
/// No separator is needed when:
/// - the anchor's last byte is already a separator (most cases:
///   `/`, `C:\`, `\\?\C:\`, `\\server\share\`, `/.nofollow/`, etc.), or
/// - the anchor is the Windows drive-relative form `<letter>:`, where
///   the `:` itself IS the boundary (`C:foo` is valid; `C:\foo` is a
///   different anchor — drive-absolute).
///
/// A separator IS needed for the other shapes whose last byte is a
/// name byte: `\\server\share`, `\\?\UNC\server\share`, `\\?\name`,
/// `/.vol/FSID/FILEID` — including degenerate cases where one of
/// those name bytes happens to be `:` (e.g. UNC share `\\server\C:`
/// or volfs FILEID ending in `:`). Those are NOT 2 bytes, so they
/// don't match the drive-relative shape.
@available(SwiftStdlib 9999, *)
internal func _anchorNeedsGapSeparator(
  _ anchorBytes: some BidirectionalCollection<FilePath.CodeUnit>
) -> Bool {
  guard let last = anchorBytes.last else { return false }
  if _isSeparator(last) { return false }
  if _isDriveRelativeAnchor(anchorBytes) { return false }
  return true
}

// MARK: - Root parsing

@available(SwiftStdlib 9999, *)
extension _SystemString {
  internal func _parseRoot() -> (
    rootEnd: Index, relativeBegin: Index
  ) {
    let result: (rootEnd: Index, relativeBegin: Index)

    if isEmpty {
      result = (startIndex, startIndex)
    } else if _isWindows {
      result = _parseWindowsRoot()
    } else if !_isSeparator(self.first!) {
      result = (startIndex, startIndex)
    } else if _isDarwin, let darwinAnchor = _parseDarwinAnchor() {
      result = (darwinAnchor.anchorEnd, darwinAnchor.relativeBegin)
    } else {
      let next = self.index(after: startIndex)
      result = (next, next)
    }

    _internalInvariant(result.rootEnd >= startIndex && result.rootEnd <= endIndex)
    _internalInvariant(result.relativeBegin >= result.rootEnd)
    _internalInvariant(result.relativeBegin <= endIndex)
    // Gap between rootEnd and relativeBegin is at most one separator
    _internalInvariant(distance(from: result.rootEnd, to: result.relativeBegin) <= 1)
    return result
  }
}

// MARK: - Separator normalization

@available(SwiftStdlib 9999, *)
extension _SystemString {
  // Coalesce repeated separators in place. On Windows, also convert `/`
  // to `\` (verbatim-aware) and prenormalize roots before coalescing.
  // Trailing separators are preserved.
  internal mutating func _normalizeSeparators() {
    guard !isEmpty else { return }
    var (writeIdx, readIdx) = (startIndex, startIndex)

    if _isWindows {
      // Detect exact \\?\ prefix on raw input before any conversion.
      // Only exact backslashes trigger verbatim mode.
      let verbatimAnchorEnd = _findVerbatimAnchorEnd()
      if verbatimAnchorEnd != startIndex {
        // Verbatim: no slash conversion in component region.
        // Anchor region is already all-backslash (required by prefix).
      } else {
        self._replaceAll(_genericSeparator, with: _platformSeparator)
        // //?/ normalizes to \\?\ after conversion, but it's
        // device-namespace, not verbatim. Demote ? → . sigil.
        if _startsWithVerbatimPrefix() != nil {
          self[index(startIndex, offsetBy: 2)] = ._dot
        }
      }
      readIdx = _prenormalizeWindowsRoots()
      writeIdx = readIdx

      while readIdx < endIndex && _isSeparator(self[readIdx]) {
        self.formIndex(after: &readIdx)
      }
    }

    while readIdx < endIndex {
      _internalInvariant(writeIdx <= readIdx)

      let wasSeparator = _isSeparator(self[readIdx])
      self.swapAt(writeIdx, readIdx)
      self.formIndex(after: &writeIdx)
      self.formIndex(after: &readIdx)

      while wasSeparator, readIdx < endIndex, _isSeparator(self[readIdx]) {
        self.formIndex(after: &readIdx)
      }
    }
    _internalInvariant(readIdx == endIndex)
    self.removeLast(self.distance(from: writeIdx, to: readIdx))
  }
}

// MARK: - Dot normalization (new rules for SE-0529)

@available(SwiftStdlib 9999, *)
extension _SystemString {
  // Append the dot-normalized form of `self[range]` to `result`, per the
  // proposal rules:
  // - `.` is dropped unless it is the leading component of an unrooted path
  // - Trailing `.` becomes a trailing separator (foo/. -> foo/)
  // - `..` is always preserved
  //
  // `range` is the relative portion to normalize — anchor and gap bytes,
  // if any, must already be in `result`. Verbatim Windows paths (where
  // `.` and `..` are regular component names) skip this entirely; the
  // caller copies bytes verbatim instead.
  //
  // `isRooted` controls leading-dot behavior: a leading `.` is dropped
  // when the path is rooted, kept when not.
  //
  // Returns `true` iff at least one component byte was appended. Callers
  // use this to roll back a speculatively-inserted anchor/relative
  // separator when the relative portion dot-normalizes to empty.
  internal func _normalizeDots(
    over range: Range<Index>,
    isRooted: Bool,
    into result: inout _SystemString
  ) -> Bool {
    // Precondition: the caller has already placed any anchor + gap bytes
    // into `result`, so the range covers the relative portion only — never
    // starts on a separator. (For Windows UNC, this is the difference
    // between `rootEnd` and `relativeBegin`.)
    _internalInvariant(
      range.lowerBound >= startIndex && range.upperBound <= endIndex)
    _internalInvariant(
      range.isEmpty || !_isSeparator(self[range.lowerBound]),
      "_normalizeDots range must start past any gap separator")

    var readIdx = range.lowerBound
    let end = range.upperBound
    var componentIndex = 0
    var emittedAny = false
    var lastDroppedADot = false
    var sourceHadTrailingSep = false

    while readIdx < end {
      // Skip a separator. If it is the last byte of the range, remember
      // that the source had a trailing separator.
      if _isSeparator(self[readIdx]) {
        let next = index(after: readIdx)
        if next >= end {
          sourceHadTrailingSep = true
        }
        readIdx = next
        continue
      }
      // Read one component span.
      let compStart = readIdx
      while readIdx < end && !_isSeparator(self[readIdx]) {
        readIdx = index(after: readIdx)
      }
      let compEnd = readIdx
      let compLen = distance(from: compStart, to: compEnd)
      let isDot = compLen == 1 && self[compStart] == ._dot

      // Drop a `.` unless it is the leading component of an unrooted path.
      let drop = isDot && !(componentIndex == 0 && !isRooted)
      if drop {
        lastDroppedADot = true
      } else {
        if emittedAny {
          result.append(_platformSeparator)
        }
        result.append(contentsOf: self[compStart..<compEnd])
        emittedAny = true
        lastDroppedADot = false
      }
      componentIndex += 1
    }

    if (sourceHadTrailingSep || lastDroppedADot) && emittedAny {
      result.append(_platformSeparator)
    }
    return emittedAny
  }
}
