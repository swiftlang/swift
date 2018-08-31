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

/// A wrapper around any `_NSArrayCore` that gives it
/// `Collection` conformance.  Why not make
/// `_NSArrayCore` conform directly?  It's a class, and I
/// don't want to pay for the dynamic dispatch overhead.
@usableFromInline
@_fixed_layout
internal struct _CocoaArrayWrapper : RandomAccessCollection {
  typealias Indices = Range<Int>
  @inlinable
  internal var startIndex: Int {
    return 0
  }

  @inlinable
  internal var endIndex: Int {
    return buffer.count
  }

  @inlinable
  internal subscript(i: Int) -> AnyObject {
    return buffer.objectAt(i)
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
  @inlinable
  internal func contiguousStorage(
    _ subRange: Range<Int>
  ) -> UnsafeMutablePointer<AnyObject>?
  {
    _sanityCheck(!subRange.isEmpty)
    var enumerationState = _makeSwiftNSFastEnumerationState()

    // This function currently returns nil unless the first
    // subRange.upperBound items are stored contiguously.  This is an
    // acceptable conservative behavior, but could potentially be
    // optimized for other cases.
    let contiguousCount = withUnsafeMutablePointer(to: &enumerationState) {
      self.buffer.countByEnumerating(with: $0, objects: nil, count: 0)
    }
    
    return contiguousCount >= subRange.upperBound
      ? UnsafeMutableRawPointer(enumerationState.itemsPtr!)
          .assumingMemoryBound(to: AnyObject.self)
        + subRange.lowerBound
      : nil
  }

  @usableFromInline @_transparent
  internal init(_ buffer: _NSArrayCore) {
    self.buffer = buffer
  }

  @usableFromInline
  internal var buffer: _NSArrayCore
}
#endif
