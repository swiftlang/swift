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
  static var breadcrumbStride: Int { return 32 }

  var utf16Length: Int

  // TODO: does this need to be a pair?.... Can we be smaller than Int?
  var crumbs: [String.Index]

  // TODO: Does this need to be inout, unique, or how will we be enforcing
  // atomicity?
  init(_ str: String) {
    let stride = _StringBreadcrumbs.breadcrumbStride

    self.crumbs = []

    if str.isEmpty {
      self.utf16Length = 0
      return
    }

    self.crumbs.reserveCapacity(
      (str._guts.count / 3) / stride)

    // TODO(UTF8 perf): More efficient implementation

    let utf16 = str.utf16
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
    _sanityCheck(self.crumbs.count == 1 + (self.utf16Length / stride))
  }
}

extension _StringBreadcrumbs {
  var stride: Int {
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
    var lowerBound = idx.encodedOffset / 3 / stride
    var upperBound = Swift.min(1 + (idx.encodedOffset / stride), crumbs.count)
    _sanityCheck(crumbs[lowerBound] <= idx)
    _sanityCheck(upperBound == crumbs.count || crumbs[upperBound] >= idx)

    while (upperBound &- lowerBound) > 1 {
      let mid = lowerBound + ((upperBound &- lowerBound) / 2)
      if crumbs[mid] <= idx { lowerBound = mid } else { upperBound = mid }
    }

    let crumb = crumbs[lowerBound]
    _sanityCheck(crumb <= idx)
    _sanityCheck(lowerBound == crumbs.count-1 || crumbs[lowerBound+1] > idx)

    return (crumb, lowerBound &* stride)
  }
}

extension _StringGuts {
  @_effects(releasenone)
  internal func getBreadcrumbsPtr() -> UnsafePointer<_StringBreadcrumbs> {
    _sanityCheck(hasBreadcrumbs)

    let mutPtr: UnsafeMutablePointer<_StringBreadcrumbs?>
    if hasNativeStorage {
      mutPtr = _object.nativeStorage._breadcrumbsAddress
    } else {
      mutPtr = UnsafeMutablePointer(
        Builtin.addressof(&_object.sharedStorage._breadcrumbs))
    }

    if _slowPath(mutPtr.pointee == nil) {
      populateBreadcrumbs(mutPtr)
    }

    _sanityCheck(mutPtr.pointee != nil)
    return UnsafePointer(mutPtr)
  }

  @inline(never) // slow-path
  @_effects(releasenone)
  internal func populateBreadcrumbs(
    _ mutPtr: UnsafeMutablePointer<_StringBreadcrumbs?>
  ) {
    // Thread-safe compare-and-swap
    let crumbs = _StringBreadcrumbs(String(self))
    _stdlib_atomicInitializeARCRef(
      object: UnsafeMutablePointer(mutPtr), desired: crumbs)
  }
}
