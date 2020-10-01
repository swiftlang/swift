//===--- CocoaArray.swift - A subset of the NSArray interface -------------===//
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
//
//  To implement bridging, the core standard library needs to interact
//  a little bit with Cocoa.  Because we want to keep the core
//  decoupled from the Foundation module, we can't use NSArray
//  directly.  We _can_, however, use an @objc protocol with a
//  compatible API.  That's _NSArrayCore.
//
//===----------------------------------------------------------------------===//

#if _runtime(_ObjC)
import SwiftShims

/// A wrapper around any `_NSArrayCore` (represented as AnyObject) that gives it
/// `Collection` conformance.  Why not make `_NSArrayCore` conform directly?
/// It's a class, and I don't want to pay for the dynamic dispatch overhead.
@usableFromInline
@frozen
internal struct _CocoaArrayWrapper: RandomAccessCollection {
  @usableFromInline
  typealias Indices = Range<Int>

  @usableFromInline
  internal var buffer: AnyObject

  @usableFromInline @_transparent
  internal init(_ buffer: AnyObject) {
    self.buffer = buffer
  }

  internal var core: _NSArrayCore {
    @inline(__always) get {
      return unsafeBitCast(buffer, to: _NSArrayCore.self)
    }
  }

  @inlinable
  internal var startIndex: Int {
    return 0
  }

  @usableFromInline
  internal var endIndex: Int {
    return core.count
  }

  @usableFromInline
  internal subscript(i: Int) -> AnyObject {
    return core.objectAt(i)
  }

  @usableFromInline
  internal subscript(bounds: Range<Int>) -> _SliceBuffer<AnyObject> {
    let boundsCount = bounds.count
    if boundsCount == 0 {
      return _SliceBuffer(
        _buffer: _ContiguousArrayBuffer<AnyObject>(),
        shiftedToStartIndex: bounds.lowerBound)
    }

    // Look for contiguous storage in the NSArray
    let cocoaStorageBaseAddress = self.contiguousStorage(self.indices)

    if let cocoaStorageBaseAddress = cocoaStorageBaseAddress {
      return _SliceBuffer(
        owner: self.buffer,
        subscriptBaseAddress: cocoaStorageBaseAddress,
        indices: bounds,
        hasNativeBuffer: false)
    }

    // No contiguous storage found; we must allocate
    let result = _ContiguousArrayBuffer<AnyObject>(
      _uninitializedCount: boundsCount,
      minimumCapacity: 0)

    // Tell Cocoa to copy the objects into our storage
    core.getObjects(
      UnsafeMutableRawPointer(result.firstElementAddress)
      .assumingMemoryBound(to: AnyObject.self),
      range: _SwiftNSRange(location: bounds.lowerBound, length: boundsCount))

    return _SliceBuffer(_buffer: result, shiftedToStartIndex: bounds.lowerBound)
  }

  /// Returns a pointer to the first element in the given non-empty `subRange`
  /// if the subRange is stored contiguously. Otherwise, return `nil`.
  ///
  /// The "non-empty" condition saves a branch within this method that can
  /// likely be better handled in a caller.
  ///
  /// - Note: This method should only be used as an optimization; it
  ///   is sometimes conservative and may return `nil` even when
  ///   contiguous storage exists, e.g., if array doesn't have a smart
  /// implementation of countByEnumerating.
  internal func contiguousStorage(
    _ subRange: Range<Int>
  ) -> UnsafeMutablePointer<AnyObject>?
  {
    _internalInvariant(!subRange.isEmpty)
    var enumerationState = _makeSwiftNSFastEnumerationState()

    // This function currently returns nil unless the first
    // subRange.upperBound items are stored contiguously.  This is an
    // acceptable conservative behavior, but could potentially be
    // optimized for other cases.
    let contiguousCount = withUnsafeMutablePointer(to: &enumerationState) {
      core.countByEnumerating(with: $0, objects: nil, count: 0)
    }

    return contiguousCount >= subRange.upperBound
      ? UnsafeMutableRawPointer(enumerationState.itemsPtr!)
          .assumingMemoryBound(to: AnyObject.self)
        + subRange.lowerBound
      : nil
  }

  @usableFromInline
  __consuming internal func _copyContents(
    subRange bounds: Range<Int>,
    initializing target: UnsafeMutablePointer<AnyObject>
  ) -> UnsafeMutablePointer<AnyObject> {
    let nsSubRange = SwiftShims._SwiftNSRange(
      location: bounds.lowerBound,
      length: bounds.upperBound - bounds.lowerBound)

    // Copies the references out of the NSArray without retaining them
    core.getObjects(target, range: nsSubRange)

    // Make another pass to retain the copied objects
    var result = target
    for _ in bounds {
      result.initialize(to: result.pointee)
      result += 1
    }
    return result
  }
}
#endif
