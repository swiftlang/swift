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

  // TODO: does this need to be a pair?.... Can we be smaller than Int?
  internal var crumbs: [String.Index]

  // TODO: Does this need to be inout, unique, or how will we be enforcing
  // atomicity?
  internal init(_ str: String, precalculatedUTF16Count: Int? = nil) {
    self.crumbs = []

    if str.isEmpty {
      self.utf16Length = 0
      return
    }

    let stride = _StringBreadcrumbs.breadcrumbStride
    let utf16 = str.utf16

    _internalInvariant(str._guts.isFastUTF8)

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
    self.crumbs.reserveCapacity(1 &+ (utf16Length / stride))

    str._guts.withFastUTF8 { utf8 in
      var crumb = utf16.startIndex
      self.crumbs.append(crumb)
      var remaining = utf16Length
      while remaining >= stride {
        crumb = unsafe utf16._nativeIndex(utf8, from: crumb, offsetBy: stride)
        self.crumbs.append(crumb)
        remaining &-= stride
      }
    }
#else
    self.crumbs.reserveCapacity(
      (str._guts.count / 3) / stride)

    var i = 0
    var curIdx = utf16.startIndex
    while curIdx != utf16.endIndex {
      if i % stride == 0 { //i.isMultiple(of: stride) {
        self.crumbs.append(curIdx)
      }
      i = i &+ 1
      curIdx = utf16.index(after: curIdx)
    }

    // Corner case: index(_:offsetBy:) can produce the endIndex
    if i % stride == 0 {
      self.crumbs.append(utf16.endIndex)
    }

    self.utf16Length = i
#endif

    _internalInvariant(self.crumbs.count == 1 + (self.utf16Length / stride))
    _invariantCheck(for: str)
  }
}

extension _StringBreadcrumbs {
  internal var stride: Int {
    @inline(__always) get { return _StringBreadcrumbs.breadcrumbStride }
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
    var lowerBound = idx._encodedOffset / 3 / stride
    var upperBound = Swift.min(1 + (idx._encodedOffset / stride), crumbs.count)
    _internalInvariant(crumbs[lowerBound] <= idx)
    _internalInvariant(upperBound == crumbs.count || crumbs[upperBound] >= idx)

    while (upperBound &- lowerBound) > 1 {
      let mid = lowerBound + ((upperBound &- lowerBound) / 2)
      if crumbs[mid] <= idx { lowerBound = mid } else { upperBound = mid }
    }

    let crumb = crumbs[lowerBound]
    _internalInvariant(crumb <= idx)
    _internalInvariant(lowerBound == crumbs.count-1 || crumbs[lowerBound+1] > idx)

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

