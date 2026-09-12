/*
 This source file is part of the Swift.org open source project

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
 See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
*/

/// A file path is a null-terminated sequence of bytes that represents
/// a location in the file system.
@available(SwiftStdlib 9999, *)
public struct FilePath: Sendable {
  internal var _storage: _SystemString

  /// Creates an empty file path.
  @available(SwiftStdlib 9999, *)
  public init() {
    self._storage = _SystemString()
  }

  internal init(_storage: _SystemString) {
    self._storage = _storage
  }
}

@available(SwiftStdlib 9999, *)
extension FilePath {
  // Normalizing init: the funnel for all path construction.
  //
  // All three platforms coalesce separators first, then parse. Darwin
  // additionally canonicalizes the anchor and excludes the resource-fork
  // suffix from dot-normalization (see _normalizeDarwin).
  internal init(_normalizing str: _SystemString) {
    if _isDarwin {
      self = _normalizeDarwin(str)
    } else if _isWindows {
      self = _normalizeWindows(str)
    } else {
      self = _normalizeLinux(str)
    }
  }

  /// The platform's canonical directory separator, as a code unit.
  ///
  /// On Linux and Darwin, this is the code unit for `/`.
  /// On Windows, it is the code unit for `\`.
  @available(SwiftStdlib 9999, *)
  public static var separator: FilePath.CodeUnit {
    _platformSeparator
  }

  /// Whether this path is empty.
  @available(SwiftStdlib 9999, *)
  public var isEmpty: Bool { _storage.isEmpty }
}

// MARK: - Per-platform normalization

@available(SwiftStdlib 9999, *)
private func _normalizeLinux(_ str: _SystemString) -> FilePath {
  _internalInvariant(_isLinux)
  var s = str
  s._normalizeSeparators()
  let (rootEnd, relBegin) = s._parseRoot()
  let isRooted = rootEnd != s.startIndex
  var result = _SystemString()
  result.append(contentsOf: s[s.startIndex..<relBegin])  // anchor + gap
  _ = s._normalizeDots(
    over: relBegin..<s.endIndex, isRooted: isRooted, into: &result)
  return FilePath(_storage: result)
}

@available(SwiftStdlib 9999, *)
private func _normalizeWindows(_ str: _SystemString) -> FilePath {
  var s = str
  s._normalizeSeparators()
  let isVerbatim = _isVerbatimComponentPath(s)
  let (rootEnd, relBegin) = s._parseRoot()
  // The only non-rooted Windows anchor is the 2-byte drive-relative `C:`;
  // empty/no-root counts as not rooted. Every other anchor (`\`, `C:\`,
  // UNC, verbatim, …) is rooted.
  let isRooted = rootEnd != s.startIndex
    && !_isDriveRelativeAnchor(s[s.startIndex..<rootEnd])
  var result = _SystemString()
  result.append(contentsOf: s[s.startIndex..<relBegin])  // anchor + gap
  if isVerbatim {
    // Verbatim paths: `.` and `..` are regular component names.
    result.append(contentsOf: s[relBegin..<s.endIndex])
  } else {
    _ = s._normalizeDots(
      over: relBegin..<s.endIndex, isRooted: isRooted, into: &result)
  }
  return FilePath(_storage: result)
}

@available(SwiftStdlib 9999, *)
private func _normalizeDarwin(_ str: _SystemString) -> FilePath {
  // Coalesce separators across the whole string first, then canonicalize
  // the anchor and parse the anchor / resource-fork suffix boundaries on
  // those coalesced bytes the way XNU classifies them.
  //
  // This is deliberately *not* what XNU does byte-for-byte: the kernel
  // does not coalesce separators before recognizing the
  // .vol/.resolve/.nofollow anchors. Because we coalesce first, our
  // canonicalization is XNU's modulo separator-coalescing — spellings
  // that differ only in runs of separators store identically. e.g.
  // /.resolve//1/foo and /.resolve/1/foo both coalesce and canonicalize
  // to /.nofollow/foo.
  var s = str
  s._normalizeSeparators()
  s._canonicalizeDarwinAnchor()

  // Parse the anchor and resource-fork suffix on the
  // coalesced+canonicalized string.
  let (rootEnd, relBegin) = s._parseRoot()
  let hasAnchor = rootEnd != s.startIndex
  var suffixStart = s._resourceForkSuffixStart ?? s.endIndex
  // If the suffix overlaps the anchor region, it's not a real suffix.
  if suffixStart < relBegin {
    suffixStart = s.endIndex
  }

  // TODO(post-PR): Single pass instead of both `s` and `result` copies.

  // Reassemble: anchor + gap + dot-normalized relative + suffix —
  // appending the relative portion into `result`
  var result = _SystemString()
  result.append(contentsOf: s[..<relBegin])

  // If the anchor doesn't already end in `/` and there is no gap
  // separator, we may need to insert one between anchor and relative.
  // Insert speculatively; roll back if the relative dot-normalizes to
  // empty.
  let needsAnchorSep =
    hasAnchor && rootEnd == relBegin
    && s[s.index(before: rootEnd)] != ._slash
  if needsAnchorSep {
    result.append(._slash)
  }

  let didEmitRelative = s._normalizeDots(
    over: relBegin..<suffixStart, isRooted: hasAnchor, into: &result)

  if needsAnchorSep && !didEmitRelative {
    _internalInvariant(result.last == ._slash)
    result.removeLast()
  }

  // Strip a trailing separator on the relative portion when a suffix
  // follows it.
  let hasSuffix = suffixStart < s.endIndex
  if hasSuffix && didEmitRelative
     && _isSeparator(result[result.index(before: result.endIndex)]) {
    _internalInvariant(_isSeparator(result.last!))
    result.removeLast()
  }

  result.append(contentsOf: s[suffixStart..<s.endIndex])

  return FilePath(_storage: result)
}

// Check if a path is a verbatim-component Windows path
@available(SwiftStdlib 9999, *)
internal func _isVerbatimComponentPath(_ storage: _SystemString) -> Bool {
  guard _isWindows else { return false }
  guard let parsed = storage._parseWindowsRootInternal() else { return false }
  return parsed.isVerbatimComponent
}
