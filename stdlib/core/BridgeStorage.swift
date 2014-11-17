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
  NativeType: AnyObject,
  CocoaType/*: AnyObject*/ // FIXME: Workaround for <rdar://problem/18992875> Couldn't find conformance
> {  
  public // @testable
  typealias Native = NativeType
  
  public // @testable
  typealias Cocoa = CocoaType
  
  public // @testable
  init(_ native: Native, bits: Int) {
    _sanityCheck(_usesNativeSwiftReferenceCounting(NativeType.self))
  
    // FIXME: Workaround for <rdar://problem/18992875> Couldn't find conformance
    // _sanityCheck(!_usesNativeSwiftReferenceCounting(CocoaType.self))
    _sanityCheck(0..<3 ~= bits,
        "BridgeStorage can't store bits outside the range 0..<3")

    rawValue = _makeNativeBridgeObject(
      native, (UInt(bits) + 1) << _objectPointerLowSpareBitShift)
  }
  
  public // @testable
  init(_ cocoa: Cocoa) {
    _sanityCheck(_usesNativeSwiftReferenceCounting(NativeType.self))
    // FIXME: Workaround for <rdar://problem/18992875> Couldn't find conformance
    // _sanityCheck(!_usesNativeSwiftReferenceCounting(CocoaType.self))
    rawValue = _makeObjCBridgeObject(
      Builtin.reinterpretCast(cocoa) as AnyObject)
  }
  
  public // @testable
  var native: Native? {
    return _nonPointerBits(rawValue) == 0
      ? nil : Builtin.castReferenceFromBridgeObject(rawValue) as Native
  }
  
  public // @testable
  var cocoa: Cocoa? {
    return _nonPointerBits(rawValue) != 0
      ? nil
    : Builtin.reinterpretCast(
        Builtin.castReferenceFromBridgeObject(rawValue) as AnyObject)
      as Cocoa // FIXME: Workaround for <rdar://problem/18992875>
               // Couldn't find conformance
  }
  
  public // @testable
  var spareBits: Int {
    return Int((_nonPointerBits(rawValue) >> _objectPointerLowSpareBitShift) - 1)
  }
  
  public // @testable
  mutating func isUniquelyReferencedNative() -> Bool {
    return _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
      _bitPattern(rawValue)
    ) != 0
  }

  //===--- private --------------------------------------------------------===//
  internal let rawValue: Builtin.BridgeObject
}
