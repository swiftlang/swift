/*
 This source file is part of the SE-0529 reference implementation

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift System project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
*/

// MARK: - Platform predicates
//
// Compile-time platform selection. This reference implementation builds for a
// single platform at a time, so these fold to constants. They replace a former
// runtime platform-switch global (now deleted) that had let the test suite
// drive all three code paths on one host; the test target now carries its own
// copy of the platform enum for that purpose. The names and signatures are
// unchanged from the old predicates, so every caller compiles as-is.
internal var _isWindows: Bool {
  #if os(Windows)
  true
  #else
  false
  #endif
}
internal var _isDarwin: Bool {
  #if canImport(Darwin)
  true
  #else
  false
  #endif
}

// The separator we use for slash-based platforms
private var genericSeparator: FilePath.CodeUnit { ._slash }

internal var platformSeparator: FilePath.CodeUnit {
  _isWindows ? ._backslash : genericSeparator
}

internal func isSeparator(_ c: FilePath.CodeUnit) -> Bool {
  c == platformSeparator
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
internal func _anchorNeedsGapSeparator(
  _ anchorBytes: some BidirectionalCollection<FilePath.CodeUnit>
) -> Bool {
  guard let last = anchorBytes.last else { return false }
  if isSeparator(last) { return false }
  if _isDriveRelativeAnchor(anchorBytes) { return false }
  return true
}

// MARK: - Root parsing

extension _SystemString {
  internal func _parseRoot() -> (
    rootEnd: Index, relativeBegin: Index
  ) {
    let result: (rootEnd: Index, relativeBegin: Index)

    if isEmpty {
      result = (startIndex, startIndex)
    } else if _isWindows {
      result = _parseWindowsRoot()
    } else if !isSeparator(self.first!) {
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

extension _SystemString {
  // Normalize separators: coalesce repeated seps.
  // On Windows, convert / to \ and prenormalize roots.
  // Does NOT remove trailing separators (new behavior).
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
        self._replaceAll(genericSeparator, with: platformSeparator)
        // //?/ normalizes to \\?\ after conversion, but it's
        // device-namespace, not verbatim. Demote ? → . sigil.
        if _startsWithVerbatimPrefix() != nil {
          self[index(startIndex, offsetBy: 2)] = ._dot
        }
      }
      readIdx = _prenormalizeWindowsRoots()
      writeIdx = readIdx

      while readIdx < endIndex && isSeparator(self[readIdx]) {
        self.formIndex(after: &readIdx)
      }
    }

    while readIdx < endIndex {
      _internalInvariant(writeIdx <= readIdx)

      let wasSeparator = isSeparator(self[readIdx])
      self.swapAt(writeIdx, readIdx)
      self.formIndex(after: &writeIdx)
      self.formIndex(after: &readIdx)

      while wasSeparator, readIdx < endIndex, isSeparator(self[readIdx]) {
        self.formIndex(after: &readIdx)
      }
    }
    self.removeLast(self.distance(from: writeIdx, to: readIdx))
  }
}

// MARK: - Dot normalization (new rules for SE-0529)

extension _SystemString {
  // Drop interior `.` components per the proposal rules:
  // - `.` is dropped unless it is the first component of a non-rooted path
  // - Trailing `.` becomes trailing separator (foo/. -> foo/)
  // - `..` always preserved
  // - Verbatim Windows paths (\\?\): `.` and `..` are NOT special
  //
  // `isRooted`: whether the original path has a rooted anchor.
  // When called on an extracted relative portion (no root in storage),
  // this tells us whether the leading `.` should be dropped.
  internal mutating func _normalizeDots(
    isVerbatimComponent: Bool, isRooted: Bool
  ) {
    guard !isVerbatimComponent else { return }
    guard !isEmpty else { return }

    let (rootEnd, relStart) = _parseRoot()
    let hasRoot = rootEnd != startIndex

    // If the storage has its own root, use the passed isRooted
    // (the caller knows whether the root is actually rooted).
    // If no root in storage (relative portion only), use isRooted directly.
    let effectivelyRooted = isRooted

    // Split into components
    var components: [[FilePath.CodeUnit]] = []
    var trailingSep = false
    var idx = relStart
    while idx < endIndex {
      if isSeparator(self[idx]) {
        let next = index(after: idx)
        if next >= endIndex {
          trailingSep = true
        }
        idx = next
        continue
      }
      let compStart = idx
      while idx < endIndex && !isSeparator(self[idx]) {
        idx = index(after: idx)
      }
      components.append(Array(self[compStart..<idx]))
    }

    let dotComp: [FilePath.CodeUnit] = [._dot]
    var normalized: [[FilePath.CodeUnit]] = []
    var hadTrailingDot = false

    for (i, comp) in components.enumerated() {
      if comp == dotComp {
        if i == 0 && !effectivelyRooted {
          normalized.append(comp)
        } else {
          if i == components.count - 1 {
            hadTrailingDot = true
          }
        }
      } else {
        normalized.append(comp)
      }
    }

    if hadTrailingDot {
      trailingSep = true
    }

    // Rebuild
    var result: [FilePath.CodeUnit] = []
    if hasRoot {
      result.append(contentsOf: self[startIndex..<relStart])
    }

    for (i, comp) in normalized.enumerated() {
      if i > 0 {
        result.append(platformSeparator)
      }
      result.append(contentsOf: comp)
    }

    if trailingSep && !normalized.isEmpty {
      result.append(platformSeparator)
    }

    self = _SystemString(result)
  }
}
