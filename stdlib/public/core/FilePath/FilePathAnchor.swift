/*
 This source file is part of the SE-0529 reference implementation

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift System project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
*/

@available(SwiftStdlib 9999, *)
extension FilePath {
  /// The anchor of a file path identifies a reference point
  /// and precedes any components.
  @available(SwiftStdlib 9999, *)
  public struct Anchor: Sendable {
    internal var _path: FilePath
    internal var _end: _SystemString.Index

    internal init(_ path: FilePath, end: _SystemString.Index) {
      self._path = path
      self._end = end
    }

    internal var _slice: _SystemString.SubSequence {
      _internalInvariant(_end >= _path._storage.startIndex && _end <= _path._storage.endIndex)
      return _path._storage[_path._storage.startIndex..<_end]
    }

    /// Whether this anchor is rooted.
    @available(SwiftStdlib 9999, *)
    public var isRooted: Bool {
      // TODO: all through this file, we have this pattern. Change to guard when it improves clarity
      if !_isWindows { return true }

      // On Windows, the only non-rooted anchor is drive-relative `C:`
      // (relative to the CWD on that drive). Everything else — `\`,
      // `C:\`, `\\server\share`, `\\?\...` — is rooted.
      return !_isDriveRelativeAnchor(_slice)
    }

    // TODO: Gate the below and others by platform as the proposal now does

    /// The drive letter of this anchor, if any.
    ///
    /// Returns the single code unit preceding the colon for drive-style
    /// anchors (`C:\`, `C:`, `\\?\C:\`, `\\.\C:\`), and `nil` for UNC
    /// anchors, non-drive device anchors, and the current-drive root `\`.
    ///
    /// The value is presented as written, without case normalization.
    /// If the drive letter is an unpaired surrogate, `U+FFFD` is returned.
    ///
    /// NOTE: The proposal gates this under `#if os(Windows)`; it is kept
    /// cross-platform here so the `REVIEW_ONLY` platform simulation can
    /// exercise it. On non-Windows platforms it returns `nil`.
    @available(SwiftStdlib 9999, *)
    public var driveLetter: Unicode.Scalar? {
      if !_isWindows { return nil }

      if let parsed = _parseWindowsAnchor() {
        if let d = parsed.drive {
          return d._driveLetterScalar
        }
      }
      return nil
    }

    /// Whether this anchor uses the Windows verbatim-component form.
    @available(SwiftStdlib 9999, *)
    public var isVerbatimComponent: Bool {
      // TODO: this can be a guard along with a guard let, probably in one guard statement
      if !_isWindows { return false }
      if let parsed = _parseWindowsAnchor() {
        return parsed.isVerbatimComponent
      }
      return false
    }

    private func _parseWindowsAnchor() -> _ParsedWindowsRoot? {
      _path._storage._parseWindowsRootInternal()
    }
  }
}

// MARK: - Anchor Hashable, Comparable, descriptions

@available(SwiftStdlib 9999, *)
extension FilePath.Anchor: Hashable {
  @available(SwiftStdlib 9999, *)
  public static func == (lhs: FilePath.Anchor, rhs: FilePath.Anchor) -> Bool {
    lhs._slice.elementsEqual(rhs._slice)
  }
  @available(SwiftStdlib 9999, *)
  public func hash(into hasher: inout Hasher) {
    for c in _slice {
      hasher.combine(c)
    }
  }
}

@available(SwiftStdlib 9999, *)
extension FilePath.Anchor: Comparable {
  @available(SwiftStdlib 9999, *)
  public static func < (lhs: FilePath.Anchor, rhs: FilePath.Anchor) -> Bool {
    lhs._slice.lexicographicallyPrecedes(rhs._slice)
  }
}

@available(SwiftStdlib 9999, *)
extension FilePath.Anchor: CustomStringConvertible, CustomDebugStringConvertible {
  @available(SwiftStdlib 9999, *)
  public var description: String {
    unsafe _slice.withCodeUnits {
      unsafe $0.withMemoryRebound(to: FilePath._Encoding.CodeUnit.self) {
        unsafe String(decoding: $0, as: FilePath._Encoding.self)
      }
    }
  }
  @available(SwiftStdlib 9999, *)
  public var debugDescription: String {
    description.debugDescription
  }
}

@available(SwiftStdlib 9999, *)
extension FilePath.Anchor: ExpressibleByStringLiteral {
  /// Creates an anchor from a string literal.
  ///
  /// Precondition: the literal is non-empty, contains no `NUL`,
  /// and forms a valid anchor.
  @available(SwiftStdlib 9999, *)
  public init(stringLiteral: String) {
    guard let a = FilePath.Anchor(stringLiteral) else {
      fatalError(
        "FilePath.Anchor string literal must be non-empty,"
        + " must not contain NUL, and must form a valid anchor")
    }
    self = a
  }

  /// Creates an anchor from a string.
  ///
  /// Returns `nil` if `string` is empty, contains `NUL`, or is
  /// not a valid anchor.
  @available(SwiftStdlib 9999, *)
  public init?(_ string: String) {
    guard let path = FilePath(string) else { return nil }
    guard let anchor = path.anchor else { return nil }
    guard path.components.isEmpty && !path.hasTrailingSeparator else {
      return nil
    }
    // A named anchor form must carry its name. `FilePath.init?` is total
    // and coalesces the degenerate Windows roots — incomplete UNC (`\\`,
    // `\\server`), empty device (`\\.`/`\\.\`), and empty verbatim
    // (`\\?`/`\\?\`) — into a degraded anchor, but as a typed `Anchor`
    // value they name a volume/device/share that isn't there, so the
    // failable `Anchor` initializer rejects them. This strictness lives
    // here, in anchor validation only; `FilePath` decomposition of these
    // inputs is unchanged.
    if _isWindows && _isIncompleteWindowsNamedAnchor(anchor._slice) {
      return nil
    }
    self = anchor
  }
}

// TODO: consider de-genericizing the below, basing it on slice. that would help debug builds.

/// Returns `true` when `anchorBytes` is a Windows UNC/device/verbatim
/// anchor form that is missing its name: incomplete UNC (`\\`, `\\server`),
/// empty device (`\\.\`), or empty verbatim (`\\?\`).
///
/// This is the strictness predicate for `FilePath.Anchor.init?`. The walk is
/// local to anchor validation and shares nothing with the construction
/// parser (`_parseWindowsRootInternal` and friends), which must keep
/// coalescing these forms unchanged for `FilePath`.
///
/// A named form requires its name: UNC needs a non-empty server AND a
/// non-empty share; device (`\\.\`) needs a non-empty device name; verbatim
/// (`\\?\`) needs a non-empty component after the prefix. Traditional roots
/// (`\`, `C:`, `C:\`) carry no separate name and are never rejected here.
@available(SwiftStdlib 9999, *)
private func _isIncompleteWindowsNamedAnchor(
  _ anchorBytes: some Collection<FilePath.CodeUnit>
) -> Bool {
  let bytes = Array(anchorBytes)
  // A named form begins with the two-backslash UNC/device/verbatim prefix.
  // One leading `\` is the bare current-drive root, and `C:` / `C:\` carry a
  // drive; none of those are a name-bearing form with the name missing.
  guard bytes.count >= 2,
        bytes[0] == ._backslash, bytes[1] == ._backslash else {
    return false
  }
  var i = 2
  if i < bytes.count, bytes[i] == ._dot || bytes[i] == ._question {
    // Device (`\\.\<device>`) or verbatim (`\\?\<component>`). Separator
    // coalescing always stores the prefix backslash (`\\.\` / `\\?\`), so
    // whatever follows it is the name. An empty name => incomplete.
    i += 1
    guard i < bytes.count, bytes[i] == ._backslash else { return true }
    i += 1
    return i >= bytes.count
  }
  // UNC (`\\server\share`): require a non-empty server AND a non-empty share.
  let serverStart = i
  while i < bytes.count, bytes[i] != ._backslash { i += 1 }
  if i == serverStart { return true }         // empty server
  guard i < bytes.count else { return true }  // server but no share at all
  i += 1                                       // skip server/share separator
  return i >= bytes.count                      // empty share
}
