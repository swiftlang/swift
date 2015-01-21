//===--- BridgeStorage.swift - Discriminated storage for bridged types ----===//
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
//  Types that are bridged to Objective-C need to manage an object
//  that may be either some native class or the @objc Cocoa
//  equivalent.  Ideally we would have a Builtin type that could
//  discriminate between these two possibilities and store a few extra
//  bits when the stored type is native.  _BridgeStorage is an
//  abstraction layer that will allow us to seamlessly integrate that
//  type, when it becomes available.
//
//===----------------------------------------------------------------------===//
import SwiftShims

public // @testable
struct _BridgeStorage<
  NativeClass: AnyObject, ObjCClass: AnyObject
> {
  public // @testable
  typealias Native = NativeClass
  
  public // @testable
  typealias ObjC = ObjCClass
  
  @inline(__always)
  public // @testable
  init(native: Native, bits: Int) {
    _sanityCheck(_usesNativeSwiftReferenceCounting(NativeClass.self))
    _sanityCheck(0..<3 ~= bits,
        "BridgeStorage can't store bits outside the range 0..<3")

    rawValue = _makeNativeBridgeObject(
      native, UInt(bits) << _objectPointerLowSpareBitShift)
  }
  
  @inline(__always)
  public // @testable
  init(objC: ObjC) {
    _sanityCheck(_usesNativeSwiftReferenceCounting(NativeClass.self))
    rawValue = _makeObjCBridgeObject(objC)
  }
  
  @inline(__always)
  public // @testable
  init(native: Native) {
    _sanityCheck(_usesNativeSwiftReferenceCounting(NativeClass.self))
    rawValue = Builtin.reinterpretCast(native)
  }
  
  public // @testable
  var spareBits: Int {
  @inline(__always) get {
    _sanityCheck(isNative)
    return Int(
      _nonPointerBits(rawValue) >> _objectPointerLowSpareBitShift)
    }
  }
  
  @inline(__always)
  public // @testable
  mutating func isUniquelyReferencedNative() -> Bool {
    return _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
      _bitPattern(rawValue)
    )
  }

  @inline(__always)
  public // @testable
  mutating func isUniquelyReferencedOrPinnedNative() -> Bool {
    return _swift_isUniquelyReferencedOrPinnedNonObjC_nonNull_bridgeObject(
      _bitPattern(rawValue)
    )
  }

  public // @testable
  var isNative: Bool {
    @inline(__always) get {
      return !_isTagged && _nonPointerBits(rawValue) != _objectPointerSpareBits
    }
  }
  
  public // @testable
  var isObjC: Bool {
    @inline(__always) get {
      return _isTagged || _nonPointerBits(rawValue) == _objectPointerSpareBits
    }
  }
  
  public // @testable
  var nativeInstance: Native {
    @inline(__always) get {
      _sanityCheck(isNative)
      return Builtin.castReferenceFromBridgeObject(rawValue)
    }
  }
  
  public // @testable
  var nativeInstance_noSpareBits: Native {
    @inline(__always) get {
      _sanityCheck(isNative)
      _sanityCheck(_nonPointerBits(rawValue) == 0)
      return Builtin.reinterpretCast(rawValue)
    }
  }
  
  @inline(__always)
  public // @testable
  mutating func isUniquelyReferenced_native_noSpareBits() -> Bool {
    _sanityCheck(isNative)
    _sanityCheck(_nonPointerBits(rawValue) == 0)
    let p: UnsafePointer<HeapObject> = Builtin.reinterpretCast(rawValue)
    return _swift_isUniquelyReferenced_nonNull_native(p)
  }

  @inline(__always)
  public // @testable
  mutating func isUniquelyReferencedOrPinned_native_noSpareBits() -> Bool {
    _sanityCheck(isNative)
    _sanityCheck(_nonPointerBits(rawValue) == 0)
    let p: UnsafePointer<HeapObject> = Builtin.reinterpretCast(rawValue)
    return _swift_isUniquelyReferencedOrPinned_nonNull_native(p)
  }

  public // @testable
  var objCInstance: ObjC {
    @inline(__always) get {
      _sanityCheck(isObjC)
      return Builtin.castReferenceFromBridgeObject(rawValue)
    }
  }
  
  //===--- private --------------------------------------------------------===//
  internal var _isTagged: Bool {
    @inline(__always) get {
      return _isObjCTaggedPointer(
        Builtin.castReferenceFromBridgeObject(rawValue))
    }
  }
  
  internal let rawValue: Builtin.BridgeObject
}
