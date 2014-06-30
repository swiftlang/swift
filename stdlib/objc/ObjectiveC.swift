//===----------------------------------------------------------------------===//
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

@exported
import ObjectiveC

//===----------------------------------------------------------------------===//
// Objective-C Primitive Types
//===----------------------------------------------------------------------===//
// FIXME: Objective-C types belong in a layer below the Objective-C support
// libraries, not here.

extension ObjCBool : Printable {
  @public var description: String {
    return self.getLogicValue().description
  }
}

/// The Objective-C SEL type.
///
/// The Objective-C SEL type is typically an opaque pointer. Swift
/// treats it as a distinct struct type, with operations to
/// convert between C strings and selectors.
///
/// The compiler has special knowledge of this type.
@public struct Selector : StringLiteralConvertible, NilLiteralConvertible {
  var ptr : COpaquePointer

  /// Create a selector from a string.
  @public init(_ str : String) {
    ptr = str.withCString { sel_registerName($0).ptr }
  }

  /// Construct a selector from a string literal.
  @public static func convertFromExtendedGraphemeClusterLiteral(
    value: CString) -> Selector {

    return convertFromStringLiteral(value)
  }

  /// Construct a selector from a string literal.
  ///
  /// FIXME: Fast-path this in the compiler, so we don't end up with
  /// the sel_registerName call at compile time.
  @public static func convertFromStringLiteral(value: CString) -> Selector {
    return sel_registerName(value)
  }

  @public init() {
    ptr = nil
  }
  
  @transparent @public
  static func convertFromNilLiteral() -> Selector {
    return Selector()
  }
}

@public func ==(lhs: Selector, rhs: Selector) -> Bool {
  return sel_isEqual(lhs, rhs)
}

extension Selector : Equatable, Hashable {
  @public var hashValue: Int {
    return ptr.hashValue
  }
}

extension Selector : Printable {
  @public var description: String {
    if let s = String.fromCStringRepairingIllFormedUTF8(sel_getName(self)).0 {
      return s
    }
    return "<NULL>"
  }
}

extension String {
  /// Construct the C string representation of an Objective-C selector.
  @public init(_sel: Selector) {
    // FIXME: This misses the ASCII optimization.
    self = String.fromCString(sel_getName(_sel))!
  }
}

extension Selector : Reflectable {
	func getMirror() -> Mirror {
		return reflect(String(_sel: self))
	}
}

// Functions used to implicitly bridge ObjCBool types to Swift's Bool type.

@internal func _convertBoolToObjCBool(x: Bool) -> ObjCBool {
  return x
}
@internal func _convertObjCBoolToBool(x: ObjCBool) -> Bool {
  return x
}

@public func ~=(x: NSObject, y: NSObject) -> Bool {
  return x.isEqual(y)
}

//===----------------------------------------------------------------------===//
// FIXME: @autoreleasepool substitute
//===----------------------------------------------------------------------===//
@asmname("objc_autoreleasePoolPush") 
func __pushAutoreleasePool() -> COpaquePointer
@asmname("objc_autoreleasePoolPop") 
func __popAutoreleasePool(pool: COpaquePointer)

@public func autoreleasepool(code: () -> ()) {
  var pool = __pushAutoreleasePool()
  code()
  __popAutoreleasePool(pool)
}

//===----------------------------------------------------------------------===//
// Mark YES and NO unavailable.
//===----------------------------------------------------------------------===//

@availability(*, unavailable, message="Use 'Bool' value 'true' instead") @public
var YES : ObjCBool = Bool.true
@availability(*, unavailable, message="Use 'Bool' value 'false' instead") @public
var NO : ObjCBool = Bool.false

