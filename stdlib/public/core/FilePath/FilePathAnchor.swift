/*
 This source file is part of the Swift.org open source project

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
 See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
*/

@available(SwiftStdlib 9999, *)
extension FilePath {
  /// The anchor of a file path identifies a reference point
  /// and precedes any components.
  @available(SwiftStdlib 9999, *)
  public struct Anchor: Sendable {
    internal var _path: FilePath
    internal var _end: _SystemString.Index

    internal init(_path: FilePath, _end: _SystemString.Index) {
      self._path = _path
      self._end = _end
    }
  }
}

@available(SwiftStdlib 9999, *)
extension FilePath.Anchor {
  internal var _slice: _SystemString.SubSequence {
    _internalInvariant(_end >= _path._storage.startIndex && _end <= _path._storage.endIndex)
    return _path._storage[_path._storage.startIndex..<_end]
  }

  /// Whether this anchor is rooted.
  @available(SwiftStdlib 9999, *)
  public var isRooted: Bool {
    guard _isWindows else { return true }

    // On Windows, the only non-rooted anchor is drive-relative `C:`
    // (relative to the CWD on that drive). Everything else — `\`,
    // `C:\`, `\\server\share`, `\\?\...` — is rooted.
    return !_isDriveRelativeAnchor(_slice)
  }
}

#if os(Windows)
@available(SwiftStdlib 9999, *)
extension FilePath.Anchor {
  /// The drive letter of this anchor, if any.
  ///
  /// Returns the single code unit preceding the colon for drive-style
  /// anchors (`C:\`, `C:`, `\\?\C:\`, `\\.\C:\`), and `nil` for UNC
  /// anchors, non-drive device anchors, and the current-drive root `\`.
  ///
  /// The value is presented as written, without case normalization.
  /// If the drive letter is an unpaired surrogate, `U+FFFD` is returned.
  @available(SwiftStdlib 9999, *)
  public var driveLetter: Unicode.Scalar? {
    _parseWindowsAnchor()?.drive?._driveLetterScalar
  }

  /// Whether this anchor uses the Windows verbatim-component form.
  @available(SwiftStdlib 9999, *)
  public var isVerbatimComponent: Bool {
    guard let parsed = _parseWindowsAnchor() else { return false }
    return parsed.isVerbatimComponent
  }

  private func _parseWindowsAnchor() -> _ParsedWindowsRoot? {
    _path._storage._parseWindowsRootInternal()
  }
}
#endif

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
  _ anchorBytes: Slice<_SystemString>
) -> Bool {
  // A named form begins with the two-backslash UNC/device/verbatim prefix.
  // One leading `\` is the bare current-drive root, and `C:` / `C:\` carry a
  // drive; none of those are a name-bearing form with the name missing.
  var s = anchorBytes
  guard s._eat(._backslash) != nil, s._eat(._backslash) != nil else {
    return false
  }
  // Device (`\\.\<device>`) or verbatim (`\\?\<component>`). Separator
  // coalescing always stores the prefix backslash (`\\.\` / `\\?\`), so
  // whatever follows it is the name; an empty name means incomplete.
  if s._eat(if: { $0 == ._dot || $0 == ._question }) != nil {
    guard s._eat(._backslash) != nil else { return true }
    return s.isEmpty
  }
  // UNC (`\\server\share`): require a non-empty server AND a non-empty share.
  guard s._eatWhile({ $0 != ._backslash }) != nil else { return true }
  guard s._eat(._backslash) != nil else { return true }
  return s.isEmpty
}
