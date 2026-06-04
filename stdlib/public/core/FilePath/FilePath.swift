/*
 This source file is part of the SE-0529 reference implementation

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift System project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
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

  // Normalizing init: the funnel for all path construction.
  //
  // All three platforms coalesce separators first, then parse. Darwin
  // additionally canonicalizes the anchor and excludes the resource-fork
  // suffix from dot-normalization (see _normalizeDarwin).
  internal init(normalizing str: _SystemString) {
    if _isDarwin {
      self = Self._normalizeDarwin(str)
    } else if _isWindows {
      self = Self._normalizeWindows(str)
    } else {
      self = Self._normalizeLinux(str)
    }
  }

  private static func _normalizeLinux(_ str: _SystemString) -> FilePath {
    var s = str
    s._normalizeSeparators()
    let (rootEnd, _) = s._parseRoot()
    let isRooted = rootEnd != s.startIndex
    s._normalizeDots(isVerbatimComponent: false, isRooted: isRooted)
    return FilePath(_storage: s)
  }

  private static func _normalizeWindows(_ str: _SystemString) -> FilePath {
    var s = str
    s._normalizeSeparators()
    let isVerbatim = _isVerbatimComponentPath(s)
    let (rootEnd, _) = s._parseRoot()
    let hasRoot = rootEnd != s.startIndex
    let isRooted: Bool
    if hasRoot {
      let anchorLen = s.distance(from: s.startIndex, to: rootEnd)
      if anchorLen == 1 && s[s.startIndex] == ._backslash {
        isRooted = true
      } else if anchorLen == 2 && s[s.index(after: s.startIndex)] == ._colon {
        isRooted = false
      } else {
        isRooted = true
      }
    } else {
      isRooted = false
    }
    s._normalizeDots(isVerbatimComponent: isVerbatim, isRooted: isRooted)
    return FilePath(_storage: s)
  }

  private static func _normalizeDarwin(_ str: _SystemString) -> FilePath {
    // TODO: the below comment isn't quite right. We are canonicalizing separators
    // first, then we are parsing and canonicalizing the way XNU does second. XNU
    // doesn't canonicalize separators, but we're doing a modulo-separator-canonicalization
    // semantics here.

    // Darwin follows the coalescing POSIX kernel parse: coalesce the
    // whole string first, then canonicalize the anchor, then parse the
    // anchor and resource-fork suffix boundaries on those same
    // coalesced+canonicalized bytes. This makes semantically-identical
    // spellings store identically — e.g. /.resolve//1/foo and
    // /.resolve/1/foo both canonicalize to /.nofollow/foo. (The earlier
    // approach canonicalized raw bytes that still contained the double
    // slash, so canonicalization never fired and a non-canonical anchor
    // such as /.resolve/1/ could persist.)
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

    // TODO: Do the below without making a bunch of arrays and allocations

    // Slice into anchor + gap separator + relative + suffix.
    let anchorSlice = Array(s[s.startIndex..<rootEnd])
    let gapSlice = Array(s[rootEnd..<relBegin])
    let relativeChars = Array(s[relBegin..<suffixStart])
    let suffixSlice = Array(s[suffixStart..<s.endIndex])

    // Dot-normalize the relative portion only. Separators are already
    // coalesced and the suffix is excluded from this step.
    var relative = _SystemString(relativeChars)
    relative._normalizeDots(isVerbatimComponent: false, isRooted: hasAnchor)

    // Strip a trailing separator from the relative portion when a suffix
    // follows it.
    if !suffixSlice.isEmpty && !relative.isEmpty
       && isSeparator(relative.last!) {
      relative.removeLast()
    }

    // Reassemble: anchor + gap + relative + suffix.
    var result = _SystemString()
    result.append(contentsOf: anchorSlice)
    result.append(contentsOf: gapSlice)
    if !relative.isEmpty && gapSlice.isEmpty && hasAnchor {
      // Insert a separator between anchor and relative, but only if the
      // anchor doesn't already end with one.
      if let last = anchorSlice.last, last != ._slash {
        result.append(._slash)
      }
    }
    result.append(contentsOf: relative)
    result.append(contentsOf: suffixSlice)

    return FilePath(_storage: result)
  }

  /// The platform's canonical directory separator, as a code unit.
  ///
  /// On Linux and Darwin, this is the code unit for `/`.
  /// On Windows, it is the code unit for `\`.
  @available(SwiftStdlib 9999, *)
  public static var separator: FilePath.CodeUnit {
    platformSeparator
  }

  /// Whether this path is empty.
  @available(SwiftStdlib 9999, *)
  public var isEmpty: Bool { _storage.isEmpty }
}

// Check if a path is a verbatim-component Windows path
@available(SwiftStdlib 9999, *)
internal func _isVerbatimComponentPath(_ storage: _SystemString) -> Bool {
  guard _isWindows else { return false }
  guard let parsed = storage._parseWindowsRootInternal() else { return false }
  return parsed.isVerbatimComponent
}
