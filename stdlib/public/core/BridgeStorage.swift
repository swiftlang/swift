//===--- BridgeStorage.swift - Discriminated storage for bridged types ----===//
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
//  Types that are bridged to Objective-C need to manage an object
//  that may be either some native class or the @objc Cocoa
//  equivalent.  _BridgeStorage discriminates between these two
//  possibilities and stores a few extra bits when the stored type is
//  native.  It is assumed that the @objc class instance may in fact
//  be a tagged pointer, and thus no extra bits may be available.
//
//===----------------------------------------------------------------------===//
import SwiftShims

@_fixed_layout
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
    
    // More bits are available on some platforms, but it's not portable
    _sanityCheck(0...1 ~= bits,
        "BridgeStorage can't store bits outside the range 0...1")

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
    return _isUnique(&rawValue)
  }

  @inline(__always)
  public // @testable
  mutating func isUniquelyReferencedOrPinnedNative() -> Bool {
    return _isUniqueOrPinned(&rawValue)
  }

  public // @testable
  var isNative: Bool {
    @inline(__always) get {
      return (_bitPattern(rawValue) &
              (_objCTaggedPointerBits | _objectPointerIsObjCBit)) == 0
    }
  }
  
  @inline(__always)
  public // @testable
  func isNativeWithClearedSpareBits(_ bits: Int) -> Bool {
    return (_bitPattern(rawValue) &
            (_objCTaggedPointerBits | _objectPointerIsObjCBit |
             (UInt(bits)) << _objectPointerLowSpareBitShift)) == 0
  }

  public // @testable
  var isObjC: Bool {
    @inline(__always) get {
      return !isNative
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
    return _isUnique_native(&rawValue)
  }

  @inline(__always)
  public // @testable
  mutating func isUniquelyReferencedOrPinned_native_noSpareBits() -> Bool {
    _sanityCheck(isNative)
    return _isUniqueOrPinned_native(&rawValue)
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
      return (_bitPattern(rawValue) & _objCTaggedPointerBits) != 0
    }
  }

  // rawValue is passed inout to _isUnique.  Although its value
  // is unchanged, it must appear mutable to the optimizer.
  @_versioned
  internal var rawValue: Builtin.BridgeObject
}
