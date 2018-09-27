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
  
  @inlinable // FIXME(sil-serialize-all)
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
  
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  public // @testable
  init(objC: ObjC) {
    _sanityCheck(_usesNativeSwiftReferenceCounting(NativeClass.self))
    rawValue = _makeObjCBridgeObject(objC)
  }
  
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  public // @testable
  init(native: Native) {
    _sanityCheck(_usesNativeSwiftReferenceCounting(NativeClass.self))
    rawValue = Builtin.reinterpretCast(native)
  }

#if !(arch(i386) || arch(arm))
  @inlinable
  @inline(__always)
  internal init(taggedPayload: UInt) {
    rawValue = _bridgeObject(taggingPayload: taggedPayload)
  }
#endif

  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  var spareBits: Int {
  @inline(__always) get {
    _sanityCheck(isNative)
    return Int(
      _nonPointerBits(rawValue) >> _objectPointerLowSpareBitShift)
    }
  }
  
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  public // @testable
  mutating func isUniquelyReferencedNative() -> Bool {
    return _isUnique(&rawValue)
  }

  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  var isNative: Bool {
    @inline(__always) get {
      let result = Builtin.classifyBridgeObject(rawValue)
      return !Bool(Builtin.or_Int1(result.isObjCObject,
                                   result.isObjCTaggedPointer))
    }
  }
  
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  public // @testable
  func isNativeWithClearedSpareBits(_ bits: Int) -> Bool {
    let fixMask = _bridgeObjectTaggedPointerBits |
      _objCTaggedPointerBits | _objectPointerIsObjCBit
    let varMask = UInt(bits) << _objectPointerLowSpareBitShift
    return _bitPattern(rawValue) & (fixMask | varMask) == 0
  }

  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  var isObjC: Bool {
    @inline(__always) get {
      return !isNative
    }
  }
  
  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  var nativeInstance: Native {
    @inline(__always) get {
      _sanityCheck(isNative)
      return Builtin.castReferenceFromBridgeObject(rawValue)
    }
  }
  
  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  var nativeInstance_noSpareBits: Native {
    @inline(__always) get {
      _sanityCheck(isNative)
      _sanityCheck(_nonPointerBits(rawValue) == 0)
      return Builtin.reinterpretCast(rawValue)
    }
  }
  
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  public // @testable
  mutating func isUniquelyReferenced_native_noSpareBits() -> Bool {
    _sanityCheck(isNative)
    return _isUnique_native(&rawValue)
  }

  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  var objCInstance: ObjC {
    @inline(__always) get {
      _sanityCheck(isObjC)
      return Builtin.castReferenceFromBridgeObject(rawValue)
    }
  }
  
  //===--- private --------------------------------------------------------===//
  @inlinable // FIXME(sil-serialize-all)
  internal var _isTagged: Bool {
    @inline(__always) get {
      return Bool(Builtin.classifyBridgeObject(rawValue).isObjCTaggedPointer)
    }
  }

  // rawValue is passed inout to _isUnique.  Although its value
  // is unchanged, it must appear mutable to the optimizer.
  @usableFromInline
  internal var rawValue: Builtin.BridgeObject
}
