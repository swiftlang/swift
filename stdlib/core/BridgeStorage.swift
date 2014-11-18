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
  
  public // @testable
  init(_ native: Native, bits: Int) {
    _sanityCheck(_usesNativeSwiftReferenceCounting(NativeClass.self))
    _sanityCheck(0..<3 ~= bits,
        "BridgeStorage can't store bits outside the range 0..<3")

    rawValue = _makeNativeBridgeObject(
      native, (UInt(bits) &+ 1) << _objectPointerLowSpareBitShift)
  }
  
  public // @testable
  init(_ objC: ObjC) {
    _sanityCheck(_usesNativeSwiftReferenceCounting(NativeClass.self))
    rawValue = _makeObjCBridgeObject(objC)
  }
  
  public // @testable
  var spareBits: Int {
    return Int(
      (_nonPointerBits(rawValue) >> _objectPointerLowSpareBitShift) &- 1)
  }
  
  public // @testable
  mutating func isUniquelyReferencedNative() -> Bool {
    return _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
      _bitPattern(rawValue)
    ) != 0
  }

  public // @testable
  var isNative: Bool {
    return _nonPointerBits(rawValue) != 0
  }
  
  public // @testable
  var isObjC: Bool {
    return _nonPointerBits(rawValue) == 0
  }
  
  public // @testable
  var nativeInstance: Native {
    _sanityCheck(isNative)
    return Builtin.castReferenceFromBridgeObject(rawValue)
  }
  
  public // @testable
  var objCInstance: ObjC {
    _sanityCheck(isObjC)
    return Builtin.castReferenceFromBridgeObject(rawValue) 
  }
  
  //===--- private --------------------------------------------------------===//
  internal let rawValue: Builtin.BridgeObject
}
