//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// @opaque
internal final class _StringBreadcrumbs {
  /// The distance between successive breadcrumbs, measured in UTF-16 code
  /// units.
  internal static var breadcrumbStride: Int { 64 }

  internal var utf16Length: Int
  
  private var crumbs: _CrumbStorage
  
  // Has to use ContiguousArray rather than Array to fit in 8 bytes
  fileprivate enum _CrumbStorage: ~Copyable {
    
    /// The largest `_encodedOffset` a packed crumb can hold. Strings whose byte
    /// count exceeds this use `_CrumbStorage.wide` instead.
    @inline(always)
    private static var maxOffset:Int { Int(UInt32.max >> 1) }
    
    /*
     A breadcrumb records a `String.Index` every `breadcrumbStride` UTF-16 units.
     Encoding one takes up to 49 bits (a 48-bit UTF-8 byte offset plus a
     trailing-surrogate bit), but for any String shorter than 2^31 bytes it fits in
     a 32 bit `_PackedCrumb`. The rare larger String falls back to storing
     String.Index directly (see `_CrumbStorage.wide`).
     */
    fileprivate struct _PackedCrumb {
      private var _bits: UInt32

      @inline(__always)
      internal init(_ idx: String.Index) {
        self._bits = _PackedCrumb.pack(idx)
      }

      @inline(__always)
      fileprivate var packedBits: UInt32 { _bits }

      @inline(__always)
      fileprivate static func pack(_ idx: String.Index) -> UInt32 {
        let off = idx._encodedOffset
        let transcoded = idx.transcodedOffset
        _internalInvariant(off >= 0 && off <= _CrumbStorage.maxOffset)
        _internalInvariant(transcoded == 0 || transcoded == 1)
        return (UInt32(truncatingIfNeeded: off) &<< 1)
          | UInt32(truncatingIfNeeded: transcoded)
      }

      /// Rebuild the stored index. Only `_encodedOffset` and `transcodedOffset` are relevant for
      /// uses of crumbs, so the dropped alignment/encoding bits are safely reconstructed here.
      /// Only native Strings use crumbs, so the index is always `_knownUTF8`.
      @inline(__always)
      internal var index: String.Index {
        let off = Int(truncatingIfNeeded: _bits &>> 1)
        if _bits & 1 == 0 {
          return String.Index(_encodedOffset: off)._scalarAligned._knownUTF8
        }
        return String.Index(encodedOffset: off, transcodedOffset: 1)._knownUTF8
      }
    }
    
    case narrow(ContiguousArray<_PackedCrumb>)
    case wide(ContiguousArray<String.Index>)
    
    @inline(always)
    fileprivate init(forCount count: Int) {
      _internalInvariant(
        MemoryLayout<_CrumbStorage>.stride ==
        MemoryLayout<ContiguousArray<String.Index>>.stride)
      self = count <= _CrumbStorage.maxOffset ? .narrow([]) : .wide([])
    }
    
    @inline(__always)
    internal var count: Int {
      switch self {
      case .narrow(let crumbs):
        crumbs.count
      case .wide(let crumbs):
        crumbs.count
      }
    }

    @inline(__always)
    internal subscript(_ i: Int) -> String.Index {
      switch self {
      case .narrow(let crumbs):
        crumbs[i].index
      case .wide(let crumbs):
        crumbs[i]
      }
    }

    /// Binary-searches the half-open crumb range `from ..< to` for the position
    /// of the last crumb that is `<= idx`
    @inline(__always)
    internal func lowerBoundCrumb(
      for idx: String.Index, from: Int, to: Int
    ) -> Int {
      var lo = from
      var hi = to
      switch self {
      case .narrow(let crumbs):
        let key = _PackedCrumb.pack(idx)
        while (hi &- lo) > 1 {
          let mid = lo &+ ((hi &- lo) &>> 1)
          // Avoid constructing a String.Index each iteration for perf
          // packedBits compares in the same order as orderingValue would
          let le = crumbs[mid].packedBits <= key
          lo = le ? mid : lo
          hi = le ? hi : mid
        }
      case .wide(let crumbs):
        let key = idx.orderingValue
        while (hi &- lo) > 1 {
          let mid = lo &+ ((hi &- lo) &>> 1)
          let le = crumbs[mid].orderingValue <= key
          lo = le ? mid : lo
          hi = le ? hi : mid
        }
      }
      return lo
    }
    
    @inline(__always)
    internal mutating func reserveCapacity(_ n: Int) {
      switch consume self {
      case .narrow(var crumbs):
        crumbs.reserveCapacity(n)
        self = .narrow(crumbs)
      case .wide(var crumbs):
        crumbs.reserveCapacity(n)
        self = .wide(crumbs)
      }
    }

    @inline(__always)
    internal mutating func append(_ idx: String.Index) {
      switch consume self {
      case .narrow(var crumbs):
        crumbs.append(_PackedCrumb(idx))
        self = .narrow(crumbs)
      case .wide(var crumbs):
        crumbs.append(idx)
        self = .wide(crumbs)
      }
    }
  }
  
  // TODO: Does this need to be inout, unique, or how will we be enforcing
  // atomicity?
  internal init(_ str: String, precalculatedUTF16Count: Int? = nil) {
    // Initialize before any method call on `self`: the `append`/
    // `reserveCapacity` helpers require a fully-initialized `self`, and the
    // `#else` scan below mutates crumbs before it knows the final length.
    self.utf16Length = 0

    if str.isEmpty {
      crumbs = _CrumbStorage(forCount: 0)
      return
    }

    let stride = _StringBreadcrumbs.breadcrumbStride
    let utf16 = str.utf16

    _internalInvariant(str._guts.isFastUTF8)

    crumbs = _CrumbStorage(forCount: str._guts.count)

#if SWIFT_STDLIB_ENABLE_VECTOR_TYPES
    let utf16Length: Int
    if let precalculatedUTF16Count {
      // If this String was originally created from UTF16, it will already have a length value we can use
      utf16Length = precalculatedUTF16Count
      _internalInvariant(utf16Length == utf16._utf16Distance(
        from: utf16.startIndex, to: utf16.endIndex))
    } else {
      /*
       This could be optimized further to do a one-pass vectorized scan,
       but this reuses machinery from the rest of the UTF16 infrastructure
       and is still ~3x faster than the scalar version it replaced
       */
      utf16Length = utf16._utf16Distance(
        from: utf16.startIndex, to: utf16.endIndex)
    }
    self.utf16Length = utf16Length

    // One crumb at offset 0, plus one per full `stride` thereafter
    crumbs.reserveCapacity(1 &+ (utf16Length / stride))

    str._guts.withFastUTF8 { utf8 in
      var crumb = utf16.startIndex
      crumbs.append(crumb)
      var remaining = utf16Length
      while remaining >= stride {
        crumb = unsafe utf16._nativeIndex(utf8, from: crumb, offsetBy: stride)
        crumbs.append(crumb)
        remaining &-= stride
      }
    }
#else
    crumbs.reserveCapacity((str._guts.count / 3) / stride)

    var i = 0
    var curIdx = utf16.startIndex
    while curIdx != utf16.endIndex {
      if i % stride == 0 { //i.isMultiple(of: stride) {
        crumbs.append(curIdx)
      }
      i = i &+ 1
      curIdx = utf16.index(after: curIdx)
    }

    // Corner case: index(_:offsetBy:) can produce the endIndex
    if i % stride == 0 {
      crumbs.append(utf16.endIndex)
    }

    self.utf16Length = i
#endif

    _internalInvariant(crumbs.count == 1 + (self.utf16Length / stride))
    _invariantCheck(for: str)
  }
}

extension _StringBreadcrumbs {
  @inline(__always)
  internal var stride: Int {
     _StringBreadcrumbs.breadcrumbStride
  }
  
  @inline(__always)
  private var count: Int {
    crumbs.count
  }

  // Fetch the lower-bound index corresponding to the given offset, returning
  // the index and the remaining offset to adjust
  internal func getBreadcrumb(
    forOffset offset: Int
  ) -> (lowerBound: String.Index, remaining: Int) {
    return (crumbs[offset / stride], offset % stride)
  }

  // Fetch the lower-bound offset corresponding to the given index, returning
  // the lower-bound and its offset
  internal func getBreadcrumb(
    forIndex idx: String.Index
  ) -> (lowerBound: String.Index, offset: Int) {
    let initialLower = idx._encodedOffset / 3 / stride
    let initialUpper = Swift.min(1 + (idx._encodedOffset / stride), count)
    _internalInvariant(crumbs[initialLower] <= idx)
    _internalInvariant(initialUpper == count || crumbs[initialUpper] >= idx)

    let lowerBound = crumbs.lowerBoundCrumb(
      for: idx,
      from: initialLower,
      to: initialUpper
    )

    let crumb = crumbs[lowerBound]
    _internalInvariant(crumb <= idx)
    _internalInvariant(
      lowerBound == count - 1 || crumbs[lowerBound + 1] > idx)

    return (crumb, lowerBound &* stride)
  }

  #if !INTERNAL_CHECKS_ENABLED
  @nonobjc @inline(__always) internal func _invariantCheck(for str: String) {}
  #else
  @nonobjc @inline(never) @_effects(releasenone)
  internal func _invariantCheck(for str: String) {
    _internalInvariant(self.utf16Length == str.utf16._distance(
      from: str.startIndex, to: str.endIndex),
      "Stale breadcrumbs")
  }
  #endif // INTERNAL_CHECKS_ENABLED
}

