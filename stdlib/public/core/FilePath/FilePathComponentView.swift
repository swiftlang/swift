/*
 This source file is part of the Swift.org open source project

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
 See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
*/

@available(SwiftStdlib 9999, *)
extension FilePath {
  /// A bidirectional, range-replaceable collection of the
  /// components that make up a file path.
  @available(SwiftStdlib 9999, *)
  public struct ComponentView: Sendable {
    internal var _path: FilePath

    // Start of this view's contribution in _path._storage. Set at
    // creation (= source path's rootEnd at that moment), immutable
    // through mutations. Used by the splice in `FilePath.components`'s
    // `set` to copy this view's bytes into a target. _relStart may move
    // forward as absorption shifts the re-parsed anchor; _originalStart
    // does not.
    internal let _originalStart: _SystemString.Index

    // Recomputed after each `replaceSubrange`. Used for iteration.
    internal var _relStart: _SystemString.Index   // first byte of components
    internal var _relEnd: _SystemString.Index     // start of suffix (or storage end)
    internal var _suffixEnd: _SystemString.Index  // end of storage

    internal init(_path: FilePath) {
      self._path = _path
      let (rootEnd, relBegin) = _path._storage._parseRoot()
      self._originalStart = rootEnd
      self._relStart = relBegin
      self._relEnd = _path._storage._componentViewRelEnd(relBegin: relBegin)
      self._suffixEnd = _path._storage.endIndex

      _internalInvariant(_originalStart <= _relStart)
      _internalInvariant(_relStart <= _relEnd)
      _internalInvariant(_relEnd <= _suffixEnd)
      _internalInvariant(_suffixEnd == _path._storage.endIndex)
    }
  }
}

// MARK: - Index

@available(SwiftStdlib 9999, *)
extension FilePath.ComponentView {
  @available(SwiftStdlib 9999, *)
  public struct Index: Sendable, Comparable, Hashable {
    internal var _storage: _SystemString.Index

    @available(SwiftStdlib 9999, *)
    public static func < (lhs: Self, rhs: Self) -> Bool {
      lhs._storage < rhs._storage
    }

    internal init(_storage: _SystemString.Index) {
      self._storage = _storage
    }
  }
}

// MARK: - Internal helpers

@available(SwiftStdlib 9999, *)
extension _SystemString {
  /// The end of the iterable component region for `ComponentView`.
  ///
  /// Excludes structural suffixes that live in `[_relEnd, _suffixEnd)`:
  /// a Darwin resource-fork suffix (the leading `/` of `/..namedfork/rsrc`)
  /// or a trailing separator on the relative region. Returns `endIndex`
  /// when there is no such suffix.
  ///
  /// The `relBegin` guard for the trailing separator is essential: for
  /// paths like `/`, `\\server\share\`, `C:\`, the trailing byte of
  /// storage IS a separator but it belongs to the anchor/gap, not the
  /// relative region. For the resource-fork case, an overlap with the
  /// anchor region (`/..namedfork/rsrc` → `rsrcStart < relBegin`) means
  /// there are no relative components at all.
  internal func _componentViewRelEnd(
    relBegin: Index
  ) -> Index {
    if _isDarwin, let rsrcStart = _resourceForkSuffixStart {
      return rsrcStart >= relBegin ? rsrcStart : relBegin
    }
    if !isEmpty {
      let lastIdx = index(before: endIndex)
      if _isSeparator(self[lastIdx]) && lastIdx >= relBegin {
        return lastIdx
      }
    }
    return endIndex
  }
}

@available(SwiftStdlib 9999, *)
extension FilePath.ComponentView {
  // Re-derive _relStart/_relEnd/_suffixEnd from current _path._storage.
  // _originalStart is intentionally NOT touched — it's set at view
  // creation and stays put even when re-decomposition shifts the anchor.
  internal mutating func _recomputeIndices() {
    let (_, relBegin) = _path._storage._parseRoot()
    _relStart = relBegin
    _relEnd = _path._storage._componentViewRelEnd(relBegin: relBegin)
    _suffixEnd = _path._storage.endIndex
  }

  internal func _componentEnd(at pos: _SystemString.Index) -> _SystemString.Index {
    var i = pos
    while i < _relEnd && !_isSeparator(_path._storage[i]) {
      _path._storage.formIndex(after: &i)
    }
    return i
  }

  internal func _skipSeparators(from pos: _SystemString.Index) -> _SystemString.Index {
    var i = pos
    while i < _relEnd && _isSeparator(_path._storage[i]) {
      _path._storage.formIndex(after: &i)
    }
    return i
  }
}

// MARK: - BidirectionalCollection

@available(SwiftStdlib 9999, *)
extension FilePath.ComponentView: BidirectionalCollection {
  @available(SwiftStdlib 9999, *)
  public typealias Element = FilePath.Component

  @available(SwiftStdlib 9999, *)
  public var startIndex: Index {
    // Skip gap separator(s) between anchor and first component
    Index(_storage: _skipSeparators(from: _relStart))
  }

  @available(SwiftStdlib 9999, *)
  public var endIndex: Index {
    // endIndex is the end of the iterable (component) region — the start
    // of any structural suffix (a trailing separator on the relative
    // region, or a Darwin resource-fork suffix), or end of storage if
    // there is no such suffix.
    Index(_storage: _relEnd)
  }

  @available(SwiftStdlib 9999, *)
  public var isEmpty: Bool {
    startIndex == endIndex
  }

  @available(SwiftStdlib 9999, *)
  public func index(after i: Index) -> Index {
    let compEnd = _componentEnd(at: i._storage)
    let next = _skipSeparators(from: compEnd)
    return Index(_storage: next)
  }

  @available(SwiftStdlib 9999, *)
  public func index(before i: Index) -> Index {
    var idx = i._storage
    // Back up past separator(s)
    while idx > startIndex._storage
          && _isSeparator(_path._storage[_path._storage.index(before: idx)]) {
      _path._storage.formIndex(before: &idx)
    }
    // Back up past component bytes
    while idx > startIndex._storage
          && !_isSeparator(_path._storage[_path._storage.index(before: idx)]) {
      _path._storage.formIndex(before: &idx)
    }
    return Index(_storage: idx)
  }

  @available(SwiftStdlib 9999, *)
  public subscript(position: Index) -> FilePath.Component {
    let end = _componentEnd(at: position._storage)
    _internalInvariant(end > position._storage, "Component must be non-empty")
    let isVerbatim = _isVerbatimComponentPath(_path._storage)
    return FilePath.Component(
      _path: _path, _range: position._storage..<end,
      _verbatimContext: isVerbatim)
  }
}

// MARK: - RangeReplaceableCollection
//
// The anchor of the result follows from whatever the path string is
// after mutation. RRC operations splice bytes within the relative
// region; the resulting path is then re-decomposed fresh — its anchor,
// resource fork status, and trailing-separator status are whatever the
// resulting bytes parse as.

@available(SwiftStdlib 9999, *)
extension FilePath.ComponentView: RangeReplaceableCollection {
  @available(SwiftStdlib 9999, *)
  public init() {
    self.init(_path: FilePath())
  }

  @available(SwiftStdlib 9999, *)
  public mutating func replaceSubrange<C>(
    _ subrange: Range<Index>, with newElements: C
  ) where C: Collection, C.Element == FilePath.Component {
    let touchesEnd = subrange.upperBound == endIndex &&
                     !(subrange.isEmpty && newElements.isEmpty)

    // Compute byte range to splice. When the subrange touches endIndex,
    // we extend to _suffixEnd (end of storage including any suffix
    // bytes), NOT just to _relEnd. RRC operations that touch the end
    // affect the entire suffix region — `removeLast` strips a trailing
    // separator OR a resource fork; `append` replaces the suffix bytes
    // with the new component.
    let byteLower = subrange.lowerBound._storage
    let byteUpper = touchesEnd ? _suffixEnd : subrange.upperBound._storage

    if newElements.isEmpty {
      // Indices point to component starts. The range [byteLower, byteUpper)
      // covers the removed component(s) plus the joining separator that
      // follows them. The exception is `touchesEnd`: there is no following
      // component to join to, so we instead back `adjLower` over the
      // PRECEDING separator to keep the result well-formed (no dangling
      // sep after the last surviving component).
      var adjLower = byteLower
      if touchesEnd && adjLower > _relStart
         && _isSeparator(_path._storage[_path._storage.index(before: adjLower)]) {
        _path._storage.formIndex(before: &adjLower)
      }
      _path._storage.removeSubrange(adjLower..<byteUpper)
    } else {
      // Boundary separators
      let needLeadingSep: Bool
      if byteLower > _relStart {
        needLeadingSep = !_isSeparator(
          _path._storage[_path._storage.index(before: byteLower)])
      } else if _path._storage.startIndex < _relStart {
        // Inserting at the start of the relative region. Add a gap
        // separator if the anchor (plus any existing gap sep up to
        // _relStart) needs one before component bytes.
        needLeadingSep = _anchorNeedsGapSeparator(_path._storage[..<_relStart])
      } else {
        needLeadingSep = false
      }

      if touchesEnd {
        // The splice runs to end-of-storage. Truncate, then append —
        // no intermediary, no inserts. There's no trailing sep in
        // this case (touchesEnd implies the splice consumes everything
        // up to and including any existing suffix bytes).
        _path._storage.removeSubrange(byteLower..<byteUpper)
        if needLeadingSep { _path._storage.append(_platformSeparator) }
        for (i, comp) in newElements.enumerated() {
          if i > 0 { _path._storage.append(_platformSeparator) }
          _path._storage.append(contentsOf: comp._slice)
        }
      } else {
        // Middle splice — there's a tail after byteUpper that has to
        // stay put. We need a single replaceSubrange to keep the
        // tail's index arithmetic straight, which means an intermediary.
        let needTrailingSep =
          byteUpper < _relEnd && !_isSeparator(_path._storage[byteUpper])

        var bytes = _SystemString()
        if needLeadingSep { bytes.append(_platformSeparator) }
        for (i, comp) in newElements.enumerated() {
          if i > 0 { bytes.append(_platformSeparator) }
          bytes.append(contentsOf: comp._slice)
        }
        if needTrailingSep { bytes.append(_platformSeparator) }

        _path._storage.replaceSubrange(byteLower..<byteUpper, with: bytes)
      }
    }

    // Recompute the mutable trio. _originalStart does NOT move — it
    // marks where this view's contribution starts in storage, regardless
    // of how the post-mutation bytes re-decompose.
    _recomputeIndices()
  }
}

// MARK: - Hashable, Comparable

@available(SwiftStdlib 9999, *)
extension FilePath.ComponentView: Hashable {
  @available(SwiftStdlib 9999, *)
  public static func == (lhs: FilePath.ComponentView, rhs: FilePath.ComponentView) -> Bool {
    lhs.elementsEqual(rhs)
  }
  @available(SwiftStdlib 9999, *)
  public func hash(into hasher: inout Hasher) {
    for c in self {
      hasher.combine(c)
    }
  }
}

@available(SwiftStdlib 9999, *)
extension FilePath.ComponentView: Comparable {
  @available(SwiftStdlib 9999, *)
  public static func < (lhs: FilePath.ComponentView, rhs: FilePath.ComponentView) -> Bool {
    for (l, r) in zip(lhs, rhs) {
      if l < r { return true }
      if r < l { return false }
    }
    return lhs.count < rhs.count
  }
}
