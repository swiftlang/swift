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
enum _BridgeStorage<
  NativeType: AnyObject, CocoaType: AnyObject
> {
  public // @testable
  typealias Native = NativeType
  
  public // @testable
  typealias Cocoa = CocoaType
  
  public // @testable
  init(_ native: Native, bits: Int) {
    _sanityCheck(_usesNativeSwiftReferenceCounting(NativeType.self))
    _sanityCheck(!_usesNativeSwiftReferenceCounting(CocoaType.self))
    
    switch bits {
    case 0: self = .Native0(native)
    case 1: self = .Native1(native)
    case 2: self = .Native2(native)
    default:
      // on i386, we have 2 spare bits.  Other platforms have more.
      _sanityCheckFailure(
        "BridgeStorage can't store bits outside the range 0-2")
    }
  }
  
  public // @testable
  init(_ cocoa: Cocoa) {
    _sanityCheck(_usesNativeSwiftReferenceCounting(NativeType.self))
    _sanityCheck(!_usesNativeSwiftReferenceCounting(CocoaType.self))
    self = .Cocoa0(cocoa)
  }
  
  public // @testable
  var native: Native? {
    switch self {
    case Native0(let x):
      return x
    case Native1(let x):
      return x
    case Native2(let x):
      return x
    default:
      return nil
    }
  }
  
  public // @testable
  var cocoa: Cocoa? {
    switch self {
    case Cocoa0(let x):
      return x
    default:
      return nil
    }
  }
  
  public // @testable
  var spareBits: Int {
    switch self {
    case Native1:
      return 1
    case Native2:
      return 2
    default:
      return 0
    }
  }
  
  public // @testable
  mutating func isUniquelyReferenced() -> Bool {
    if let raw = native.map({ Builtin.bridgeToRawPointer($0) }) {
      return _swift_isUniquelyReferenced_nonNull_native(UnsafePointer(raw)) != 0
    }
    return false
  }
  
  //===--- private/internal -----------------------------------------------===//
case Native0(Native)
case Native1(Native)
case Native2(Native)
case Cocoa0(CocoaType)
}

