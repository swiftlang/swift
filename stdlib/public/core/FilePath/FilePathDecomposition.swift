/*
 This source file is part of the Swift.org open source project

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
 See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
*/

// MARK: - Anchor property

@available(SwiftStdlib 9999, *)
extension FilePath {
  /// The anchor of this path, if any.
  @available(SwiftStdlib 9999, *)
  public var anchor: Anchor? {
    get {
      let (rootEnd, _) = _storage._parseRoot()
      guard rootEnd != _storage.startIndex else { return nil }
      _internalInvariant(rootEnd <= _storage.endIndex)
      return Anchor(_path: self, _end: rootEnd)
    }
    set {
      let (rootEnd, relBegin) = _storage._parseRoot()
      _internalInvariant(relBegin >= rootEnd)
      if let newAnchor = newValue {
        // Replace old root region (including gap separator) with new
        // anchor bytes. If the new anchor's shape needs a gap
        // separator before existing relative content, insert one
        // afterwards rather than copying the bytes through an
        // intermediate Array.
        let hasRelativeContent = relBegin < _storage.endIndex
        let needsSep = hasRelativeContent
          && _anchorNeedsGapSeparator(newAnchor._slice)
        _storage.replaceSubrange(
          _storage.startIndex..<relBegin, with: newAnchor._slice)
        if needsSep {
          let after = _storage.index(
            _storage.startIndex, offsetBy: newAnchor._slice.count)
          _storage.insert(_platformSeparator, at: after)
        }
      } else {
        _storage.removeSubrange(_storage.startIndex..<relBegin)
      }
    }
  }
}

// MARK: - Components property

@available(SwiftStdlib 9999, *)
extension FilePath {
  /// View the relative path components that make up this path.
  ///
  /// The anchor of the result follows from whatever the path string is
  /// after mutation: re-decomposition of the resulting bytes is what the
  /// kernel sees, and is what we report.
  ///
  /// `set` splices the new view's contributed bytes
  /// (`[_originalStart, _suffixEnd)`) into self's post-anchor region —
  /// self's anchor bytes are physically untouched, so the anchor is
  /// preserved by construction. Mutation that produces bytes parsing
  /// as a different anchor (e.g. inserting `.nofollow` at the front of
  /// an absolute Darwin path) lets the new anchor stand, because the
  /// absorbed bytes are inside the spliced region.
  ///
  /// In-place mutation (`path.components.append(x)`) lowers to
  /// get-mutate-set: the temporary view's mutating method splices into
  /// its own `_path`, then `set` splices that result back into self.
  @available(SwiftStdlib 9999, *)
  public var components: ComponentView {
    get { ComponentView(_path: self) }
    set {
      let (selfRootEnd, _) = _storage._parseRoot()
      let cvBytes = newValue._path._storage[
        newValue._originalStart..<newValue._suffixEnd]

      // Truncate to just the anchor, then append the gap separator
      // (if needed) and the contribution. No inserts, no intermediary
      // — every byte in `cvBytes` is copied exactly once.
      let needsSep = !cvBytes.isEmpty
        && _anchorNeedsGapSeparator(_storage[..<selfRootEnd])
        && !_isSeparator(cvBytes.first!)
      _storage.removeSubrange(selfRootEnd..<_storage.endIndex)
      if needsSep { _storage.append(_platformSeparator) }
      _storage.append(contentsOf: cvBytes)
    }
  }
}

// MARK: - Absolute / relative

@available(SwiftStdlib 9999, *)
extension FilePath {
  /// Returns true if this path uniquely identifies the location of
  /// a file without reference to an additional starting location.
  @available(SwiftStdlib 9999, *)
  public var isAbsolute: Bool {
    guard let anchor = anchor else { return false }
    if !_isWindows { return true }

    // On Windows, only fully qualified paths are absolute
    let slice = anchor._slice
    guard slice.count >= 3 else {
      // `\` (1 char) or `C:` (2 chars) are relative
      return false
    }
    return true
  }

}

// MARK: - Trailing separator

@available(SwiftStdlib 9999, *)
extension FilePath {
  /// Whether this path ends with a directory separator that is
  /// not structurally required by the path's anchor.
  @available(SwiftStdlib 9999, *)
  public var hasTrailingSeparator: Bool {
    get {
      guard !isEmpty else { return false }
      if _storage._hasResourceForkSuffix() { return false }
      let (rootEnd, relBegin) = _storage._parseRoot()
      _internalInvariant(relBegin >= rootEnd)
      if relBegin < _storage.endIndex {
        // Has relative content; trailing sep is the last byte
        return _isSeparator(_storage[_storage.index(before: _storage.endIndex)])
      } else if relBegin > rootEnd {
        // No relative content, but a gap separator exists between
        // the anchor and the end of the string (e.g. `\\server\share\`
        // or `/.vol/1234/5678/`). That gap separator IS the trailing
        // separator.
        _internalInvariant(relBegin == _storage.endIndex)
        _internalInvariant(_isSeparator(_storage[rootEnd]))
        return true
      }
      // Anchor-only or empty root, no trailing separator
      return false
    }
    set {
      if newValue == hasTrailingSeparator { return }
      if newValue {
        // Add trailing separator
        if isEmpty { return }
        if _storage._hasResourceForkSuffix() {
          // Replace resource fork with trailing sep
          if let rsrcStart = _storage._resourceForkSuffixStart {
            _storage.removeSubrange(rsrcStart..<_storage.endIndex)
          }
        }
        if !_isSeparator(_storage.last!) {
          _storage.append(_platformSeparator)
        }
      } else {
        // Remove trailing separator
        if !isEmpty && _isSeparator(_storage.last!) {
          let (_, relBegin) = _storage._parseRoot()
          if _storage.index(before: _storage.endIndex) >= relBegin {
            _internalInvariant(_isSeparator(_storage.last!))
            _storage.removeLast()
          }
        }
      }
    }
  }

  /// Returns a copy with a trailing separator added.
  @available(SwiftStdlib 9999, *)
  public func withTrailingSeparator() -> FilePath {
    var copy = self
    copy.hasTrailingSeparator = true
    return copy
  }

  /// Returns a copy with the trailing separator removed.
  @available(SwiftStdlib 9999, *)
  public func withoutTrailingSeparator() -> FilePath {
    var copy = self
    copy.hasTrailingSeparator = false
    return copy
  }
}

// MARK: - Resource fork (Darwin)
//
// Implemented only on Darwin builds; the getter returns `false` on other
// platforms and the setter is a no-op (the helpers in FilePathDarwin.swift
// guard on `_isDarwin`, which folds to `false` at compile time elsewhere).

@available(SwiftStdlib 9999, *)
extension FilePath {
  /// Whether this path ends with a resource fork reference.
  @available(SwiftStdlib 9999, *)
  public var isResourceFork: Bool {
    get { _storage._hasResourceForkSuffix() }
    set {
      if newValue == isResourceFork { return }
      if newValue {
        // Add resource fork suffix
        if hasTrailingSeparator {
          hasTrailingSeparator = false
        }
        var suffix = _SystemString._resourceForkSuffix
        // Avoid double separator when path already ends with one
        if !_storage.isEmpty && _isSeparator(_storage.last!)
           && !suffix.isEmpty && _isSeparator(suffix.first!) {
          _internalInvariant(_isSeparator(suffix.first!))
          suffix.removeFirst()
        }
        _storage.append(contentsOf: suffix)
      } else {
        // Remove resource fork suffix
        if let rsrcStart = _storage._resourceForkSuffixStart {
          _storage.removeSubrange(rsrcStart..<_storage.endIndex)
        }
      }
    }
  }

  /// Returns a copy with resource fork suffix appended.
  @available(SwiftStdlib 9999, *)
  public func withResourceFork() -> FilePath {
    var copy = self
    copy.isResourceFork = true
    return copy
  }

  /// Returns a copy with resource fork suffix removed.
  @available(SwiftStdlib 9999, *)
  public func withoutResourceFork() -> FilePath {
    var copy = self
    copy.isResourceFork = false
    return copy
  }
}

// MARK: - Reconstruction initializers

@available(SwiftStdlib 9999, *)
extension FilePath {
  /// Creates a file path from a decomposed form.
  @available(SwiftStdlib 9999, *)
  public init(
    anchor: Anchor?,
    _ components: some Sequence<Component>,
    hasTrailingSeparator: Bool = false
  ) {
    var str = _SystemString()

    if let anchor = anchor {
      str.append(contentsOf: anchor._slice)
    }

    let comps = Array(components)

    for (i, comp) in comps.enumerated() {
      if i == 0 {
        // Insert a separator between anchor and first component if the
        // anchor's shape needs one.
        if let anchor = anchor, _anchorNeedsGapSeparator(anchor._slice) {
          str.append(_platformSeparator)
        }
      } else {
        str.append(_platformSeparator)
      }
      str.append(contentsOf: comp._slice)
    }

    if hasTrailingSeparator {
      if !comps.isEmpty {
        str.append(_platformSeparator)
      } else if anchor != nil {
        // Trailing sep on anchor-only path (e.g., \\server\share\)
        // Add separator if anchor doesn't already end with one
        if let last = anchor?._slice.last, !_isSeparator(last) {
          str.append(_platformSeparator)
        }
      }
    }

    self.init(_normalizing: str)
  }

  /// Creates a file path from a decomposed form with a resource fork suffix.
  @available(SwiftStdlib 9999, *)
  public init(
    anchor: Anchor?,
    _ components: some Sequence<Component>,
    resourceFork: Bool
  ) {
    self.init(anchor: anchor, components, hasTrailingSeparator: false)
    if resourceFork {
      self.isResourceFork = true
    }
  }
}
