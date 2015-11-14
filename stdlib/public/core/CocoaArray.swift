//===--- CocoaArray.swift - A subset of the NSArray interface -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  To implement bridging, the core standard library needs to interact
//  a little bit with Cocoa.  Because we want to keep the core
//  decoupled from the Foundation module, we can't use NSArray
//  directly.  We _can_, however, use an @objc protocol with a
//  compatible API.  That's _NSArrayCoreType.
//
//===----------------------------------------------------------------------===//

#if _runtime(_ObjC)
import SwiftShims

/// A wrapper around any `_NSArrayCoreType` that gives it
/// `CollectionType` conformance.  Why not make
/// `_NSArrayCoreType` conform directly?  It's a class, and I
/// don't want to pay for the dynamic dispatch overhead.
internal struct _CocoaArrayWrapper : CollectionType {
  var startIndex: Int {
    return 0
  }

  var endIndex: Int {
    return buffer.count
  }

  subscript(i: Int) -> AnyObject {
    return buffer.objectAtIndex(i)
  }

  /// Returns a pointer to the first element in the given subRange if
  /// the subRange is stored contiguously. Otherwise, return nil.
  ///
  /// - Note: This method should only be used as an optimization; it
  ///   is sometimes conservative and may return nil even when
  ///   contiguous storage exists, e.g., if array doesn't have a smart
  /// implementation of countByEnumeratingWithState.
  func contiguousStorage(
    subRange: Range<Int>
  ) -> UnsafeMutablePointer<AnyObject>
  {
    var enumerationState = _makeSwiftNSFastEnumerationState()

    // This function currently returns nil unless the first
    // subRange.endIndex items are stored contiguously.  This is an
    // acceptable conservative behavior, but could potentially be
    // optimized for other cases.
    let contiguousCount = withUnsafeMutablePointer(&enumerationState) {
      self.buffer.countByEnumeratingWithState($0, objects: nil, count: 0)
    }
    
    return contiguousCount >= subRange.endIndex
    ? unsafeBitCast(
      enumerationState.itemsPtr, UnsafeMutablePointer<AnyObject>.self
      ) + subRange.startIndex
    : nil
  }

  @transparent
  init(_ buffer: _NSArrayCoreType) {
    self.buffer = buffer
  }

  var buffer: _NSArrayCoreType
}
#endif
