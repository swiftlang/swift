//===--- NSSwiftArray.swift - Links NSArray and _ContiguousArrayStorage ---===//
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
//  _NSSwiftArray supplies the implementation of the _CocoaArrayType API
//  (and thus, NSArray the API) for our _ContiguousArrayStorage<T>.  We
//  can't put this implementation directly on _ContiguousArrayStorage
//  because generic classes can't override Objective-C selectors.
//
//===----------------------------------------------------------------------===//

import SwiftShims

/// Return whether the given `index` is valid, i.e. `0 ≤ index ≤ count`
@transparent
func _isValidArrayIndex(index: Int, count: Int) -> Bool {
  return (index >= 0) & (index <= count)
}

/// Return whether the given `index` is valid for subscripting, i.e.
/// `0 ≤ index < count`
@transparent
func _isValidArraySubscript(index: Int, count: Int) -> Bool {
  return (index >= 0) & (index < count)
}


/// Base class of the heap buffer backing arrays.
@objc class _NSSwiftArray : _CocoaArrayType {
  typealias Buffer = HeapBuffer<_ArrayBody, AnyObject>

  // The optional Void arguments prevent these methods from being
  // @objc, rendering them overridable by the generic class
  // _ContiguousArrayStorage<T>

  /// Returns the object located at the specified `index` when the
  /// element type is not bridged verbatim.
  func bridgingObjectAtIndex(index: Int, dummy: Void) -> AnyObject {
    _sanityCheckFailure(
      "Concrete subclasses must implement bridgingObjectAtIndex")
  }

  /// Copies the objects contained in the array that fall within the
  /// specified range to `aBuffer`.
  func bridgingGetObjects(
    aBuffer: UnsafeMutablePointer<AnyObject>,
    range: _SwiftNSRange, dummy: Void
  ) {
    _sanityCheckFailure(
      "Concrete subclasses must implement bridgingGetObjects")
  }

  func bridgingCountByEnumeratingWithState(
    state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>,
    count bufferSize: Int, dummy: Void
  ) -> Int {
    _sanityCheckFailure(
      "Concrete subclasses must implement bridgingCountByEnumeratingWithState")
  }

  func canStoreElementsOfDynamicType(_: Any.Type) -> Bool {
    _sanityCheckFailure(
      "Concrete subclasses must implement canStoreElementsOfDynamicType")
  }

  /// A type that every element in the array is.
  var staticElementType: Any.Type {
    _sanityCheckFailure(
      "Concrete subclasses must implement staticElementType")
  }

  //
  // NSArray implementation.
  //
  // Do not call any of these methods from the standard library!
  //

  var count: Int {
    return unsafeBitCast(self, Buffer.self).value.count
  }

  /// Returns the object located at the specified `index`.
  func objectAtIndex(index: Int) -> AnyObject {
    let buffer = unsafeBitCast(self, Buffer.self)
    _precondition(
      _isValidArraySubscript(index, buffer.value.count),
      "Array index out of range")

    if _fastPath(buffer.value.elementTypeIsBridgedVerbatim) {
      return buffer[index]
    }
    return bridgingObjectAtIndex(index, dummy: ())
  }

  /// Copies the objects contained in the array that fall within the
  /// specified `range` to `aBuffer`.
  func getObjects(
    aBuffer: UnsafeMutablePointer<AnyObject>, range: _SwiftNSRange
  ) {
    let buffer = unsafeBitCast(self, Buffer.self)
    _precondition(
      _isValidArrayIndex(range.location, buffer.value.count),
      "Array index out of range")
    _precondition(
      _isValidArrayIndex(range.location + range.length, buffer.value.count),
      "Array index out of range")

    if _fastPath(buffer.value.elementTypeIsBridgedVerbatim || count == 0) {
      // These objects are "returned" at +0, so treat them as values to
      // avoid retains.
      var dst = UnsafeMutablePointer<Word>(aBuffer)

      if _fastPath(buffer.value.elementTypeIsBridgedVerbatim) {
        dst.initializeFrom(
          UnsafeMutablePointer(buffer.baseAddress + range.location),
          count: range.length)
      }

      for i in range.location..<range.location + range.length {
        dst++.initialize(unsafeBitCast(buffer[i], Word.self))
      }
    }
    else {
      bridgingGetObjects(aBuffer, range: range, dummy: ())
    }
  }

  func copyWithZone(_: COpaquePointer) -> _CocoaArrayType {
    return self
  }

  func countByEnumeratingWithState(
    state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>, count bufferSize: Int
  ) -> Int {
    let buffer = unsafeBitCast(self, Buffer.self)
    if _fastPath(buffer.value.elementTypeIsBridgedVerbatim) {
      var enumerationState = state.memory

      if enumerationState.state != 0 {
        return 0
      }
      enumerationState.mutationsPtr = _fastEnumerationStorageMutationsPtr
      enumerationState.itemsPtr = unsafeBitCast(
        buffer.baseAddress, AutoreleasingUnsafeMutablePointer<AnyObject?>.self)
      enumerationState.state = 1
      state.memory = enumerationState
      return buffer.value.count
    }
    else {
      return bridgingCountByEnumeratingWithState(
        state, objects: objects, count: bufferSize, dummy: ())
    }
  }
}
