//===--- NSSwiftArray.swift - Links NSArray and NativeArrayStorage --------===//
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
//  NSSwiftArray supplies the implementation of the CocoaArray API
//  (and thus, NSArray the API) for our NativeArrayStorage<T>.  We
//  can't put this implementation directly on NativeArrayStorage
//  because generic classes can't override Objective-C selectors.
//
//===----------------------------------------------------------------------===//

import SwiftShims

// Base class of the heap buffer implementation backing the new Array
// design.  
@objc
class NSSwiftArray : HeapBufferStorageBase, CocoaArray {
  typealias Buffer = HeapBuffer<ArrayBody, AnyObject>
  
  // Note: This method must be overridden by subclasses, and would be
  // marked @abstract if we had such a feature. Moreover, those
  // subclasses must be generic, so we can't allow this method to be @objc
  // (methods of generic classes can't be @objc), so we use a tuple in the
  // signature to prevent the inference of @objc here.
  func _objectAtIndex(indexAndUnused: (Int, Bool)) -> AnyObject {
    assert(false, "_objectAtIndex must be overridden")
    return self
  }

  func objectAtIndex(index: Int) -> AnyObject {
    return _objectAtIndex((index, true))
  }
  
  func getObjects(
    objects: UnsafePointer<AnyObject>)range(range: _SwiftNSRange) {
    var dst = objects
    for i in range.location...range.location + range.length {
      dst++.initialize(_objectAtIndex((i, true)))
    }
  }

  func copyWithZone(_: COpaquePointer) -> CocoaArray {
    // Copy-on-write keeps us from modifying this as long as Cocoa can
    // see it.
    return self
  }

  func countByEnumeratingWithState(
    state: UnsafePointer<_SwiftNSFastEnumerationState>)
    objects(UnsafePointer<AnyObject>) count(bufferSize: Int)
  -> Int {
    var enumerationState = state.get()

    let my = reinterpretCast(self) as Buffer
    if _fastPath(my.value.elementTypeIsBridgedVerbatim) {
      if enumerationState.state != 0 {
        return 0
      }
      enumerationState.mutationsPtr = reinterpretCast(self)
      enumerationState.itemsPtr = reinterpretCast(my.elementStorage)
      enumerationState.state = 1
      state.set(enumerationState)
      return my.value.count
    }
    
    // FIXME: Could optimize away this copy if we have a buffer that
    // already contains AnyObjects.
    let firstItem = Int(enumerationState.state)
    let numItemsToCopy = min(self.count - firstItem, bufferSize)
    getObjects(objects, range:_SwiftNSRange(firstItem, numItemsToCopy))
    enumerationState.mutationsPtr = reinterpretCast(self)
    enumerationState.itemsPtr = reinterpretCast(objects)
    enumerationState.state += CUnsignedLong(numItemsToCopy)
    state.set(enumerationState)
    return numItemsToCopy
  }
  
  var count: Int {
    return (reinterpretCast(self) as Buffer).value.count
  }
}
