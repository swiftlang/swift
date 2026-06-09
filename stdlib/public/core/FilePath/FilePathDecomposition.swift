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
    // On Windows, only fully qualified paths are absolute. The relative
    // anchors are `\` (1 byte) and `C:` (2 bytes); 3+ bytes is absolute.
    return anchor._slice.count >= 3
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
      if !newValue {
        // Remove the trailing separator. The getter returned true, so the
        // last byte is a separator; drop it unless it's the structural gap
        // separator (when relBegin > rootEnd, e.g. `\\server\share\`),
        // which belongs to the anchor.
        let (_, relBegin) = _storage._parseRoot()
        if _storage.index(before: _storage.endIndex) >= relBegin {
          _storage.removeLast()
        }
        return
      }
      // Add a trailing separator.
      if isEmpty { return }
      if let rsrcStart = _storage._resourceForkSuffixStart {
        _storage.removeSubrange(rsrcStart..<_storage.endIndex)
      }
      if !_isSeparator(_storage.last!) {
        _storage.append(_platformSeparator)
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
      if !newValue {
        // Remove the resource fork suffix.
        if let rsrcStart = _storage._resourceForkSuffixStart {
          _storage.removeSubrange(rsrcStart..<_storage.endIndex)
        }
        return
      }
      // Add the resource fork suffix. The literal starts with `/`, so
      // when storage already ends in a separator we drop the leading `/`
      // to avoid storing two in a row.
      if hasTrailingSeparator {
        hasTrailingSeparator = false
      }
      let suffix = _SystemString._resourceForkSuffix._asciiBytes
      if !_storage.isEmpty && _isSeparator(_storage.last!) {
        _storage.append(contentsOf: suffix.dropFirst())
      } else {
        _storage.append(contentsOf: suffix)
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
    // Whether a separator is needed between the anchor and the first
    // component, when one or more components follow.
    let anchorNeedsSep = anchor.map {
      _anchorNeedsGapSeparator($0._slice)
    } ?? false

    var hasComponents = false
    for comp in components {
      // First component: separator iff the anchor's shape needs one.
      // Subsequent components: always a separator.
      if hasComponents || anchorNeedsSep {
        str.append(_platformSeparator)
      }
      str.append(contentsOf: comp._slice)
      hasComponents = true
    }

    if hasTrailingSeparator {
      if hasComponents {
        str.append(_platformSeparator)
      } else if let anchor = anchor, let last = anchor._slice.last,
                !_isSeparator(last) {
        // Trailing sep on an anchor-only path (e.g., `\\server\share\`):
        // add a separator only if the anchor doesn't already end with one.
        str.append(_platformSeparator)
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
