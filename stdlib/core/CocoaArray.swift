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
//  compatible API.  That's _CocoaArray.
//
//===----------------------------------------------------------------------===//

import SwiftShims

/// A subset of the NSArray interface with call-compatible selectors
/// (e.g. _SwiftNSRange is layout-compatible with NSRange in
/// getObjects:range: below).  Array<T> is backed by one of these, and
/// when T isBridgedToObjectiveC, it can be used directly as an
/// NSArray.  It is safe to convert between NSArray and _CocoaArray via
/// reinterpretCast.
@objc @class_protocol @unsafe_no_objc_tagged_pointer
protocol _CocoaArray {
  func objectAtIndex(index: Int) -> AnyObject
  
  func getObjects(UnsafePointer<AnyObject>, range: _SwiftNSRange)
  
  func countByEnumeratingWithState(
         state: UnsafePointer<_SwiftNSFastEnumerationState>,
         objects buffer: UnsafePointer<AnyObject>,
         count len: Int) -> Int

  func copyWithZone(COpaquePointer) -> _CocoaArray
  
  var count: Int {get}
}

/// A wrapper around any _CocoaArray that gives it Collection
/// conformance.  Why not make _CocoaArray conform directly?  It's a
/// class, and I don't want to pay for the dynamic dispatch overhead.
struct _CocoaArrayWrapper : Collection {
  var startIndex: Int {
    return 0
  }

  var endIndex: Int {
    return buffer.count
  }

  subscript(i: Int) -> AnyObject {
    return buffer.objectAtIndex(i)
  }

  func generate() -> IndexingGenerator<_CocoaArrayWrapper> {
    return IndexingGenerator(self)
  }
  
  /// Returns a pointer to the first element in the given subRange if
  /// the subRange is stored contiguously. Otherwise, return nil.
  ///
  /// Note: this method should only be used as an optimization; it
  /// is sometimes conservative and may return nil even when
  /// contiguous storage exists, e.g., if array doesn't have a smart
  /// implementation of countByEnumeratingWithState.
  func contiguousStorage(subRange: Range<Int>) -> UnsafePointer<AnyObject>
  {
    var enumerationState = _makeSwiftNSFastEnumerationState()

    // This function currently returns nil unless the first
    // subRange.endIndex items are stored contiguously.  This is an
    // acceptable conservative behavior, but could potentially be
    // optimized for other cases.
    let contiguousCount = withUnsafePointer(&enumerationState) {
      self.buffer.countByEnumeratingWithState($0, objects: nil, count: 0)
    }
    
    return contiguousCount >= subRange.endIndex
    ? reinterpretCast(enumerationState.itemsPtr) + subRange.startIndex : nil
  }

  @transparent
  init(_ buffer: _CocoaArray) {
    self.buffer = buffer
  }

  var buffer: _CocoaArray
}

