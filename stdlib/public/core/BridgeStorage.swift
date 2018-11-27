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
@usableFromInline
internal struct _BridgeStorage<
  NativeClass: AnyObject,
  ObjCClass: AnyObject
> {
  @usableFromInline
  internal typealias Native = NativeClass

  @usableFromInline
  internal typealias ObjC = ObjCClass

  // rawValue is passed inout to _isUnique.  Although its value
  // is unchanged, it must appear mutable to the optimizer.
  @usableFromInline
  internal var rawValue: Builtin.BridgeObject

  @inlinable
  @inline(__always)
  internal init(native: Native, isFlagged flag: Bool) {
    // Note: Some platforms provide more than one spare bit, but the minimum is
    // a single bit.

    _sanityCheck(_usesNativeSwiftReferenceCounting(NativeClass.self))

    rawValue = _makeNativeBridgeObject(
      native,
      flag ? (1 as UInt) << _objectPointerLowSpareBitShift : 0)
  }

  @inlinable
  @inline(__always)
  internal init(objC: ObjC) {
    _sanityCheck(_usesNativeSwiftReferenceCounting(NativeClass.self))
    rawValue = _makeObjCBridgeObject(objC)
  }

  @inlinable
  @inline(__always)
  internal init(native: Native) {
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

  @inlinable
  @inline(__always)
  internal mutating func isUniquelyReferencedNative() -> Bool {
    return _isUnique(&rawValue)
  }

  @inlinable
  internal var isNative: Bool {
    @inline(__always) get {
      let result = Builtin.classifyBridgeObject(rawValue)
      return !Bool(Builtin.or_Int1(result.isObjCObject,
                                   result.isObjCTaggedPointer))
    }
  }

  @inlinable
  static var flagMask: UInt {
    @inline(__always) get {
      return (1 as UInt) << _objectPointerLowSpareBitShift
    }
  }

  @inlinable
  internal var isUnflaggedNative: Bool {
    @inline(__always) get {
      return (_bitPattern(rawValue) &
        (_bridgeObjectTaggedPointerBits | _objCTaggedPointerBits |
          _objectPointerIsObjCBit | _BridgeStorage.flagMask)) == 0
    }
  }

  @inlinable
  internal var isObjC: Bool {
    @inline(__always) get {
      return !isNative
    }
  }

  @inlinable
  internal var nativeInstance: Native {
    @inline(__always) get {
      _sanityCheck(isNative)
      return Builtin.castReferenceFromBridgeObject(rawValue)
    }
  }

  @inlinable
  internal var unflaggedNativeInstance: Native {
    @inline(__always) get {
      _sanityCheck(isNative)
      _sanityCheck(_nonPointerBits(rawValue) == 0)
      return Builtin.reinterpretCast(rawValue)
    }
  }

  @inlinable
  @inline(__always)
  internal mutating func isUniquelyReferencedUnflaggedNative() -> Bool {
    _sanityCheck(isNative)
    return _isUnique_native(&rawValue)
  }

  @inlinable
  internal var objCInstance: ObjC {
    @inline(__always) get {
      _sanityCheck(isObjC)
      return Builtin.castReferenceFromBridgeObject(rawValue)
    }
  }
}
