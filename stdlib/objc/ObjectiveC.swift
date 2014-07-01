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

/// The Objective-C BOOL type.
///
/// On iOS, the Objective-C BOOL type is a typedef of C/C++ bool. Clang 
/// importer imports it as ObjCBool.
///
/// The compiler has special knowledge of this type.
///
/// FIXME: It may be possible to replace this with `typealias ObjCBool = Bool`.
@public struct ObjCBool {
#if os(OSX) || (os(iOS) && (arch(i386) || arch(arm)))
  // On OS X and 32-bit iOS, Objective-C's BOOL type is a "signed char".
  var value: Int8

  @public init(_ value: Int8) {
    self.value = value
  }
#else
  // Everywhere else it is C/C++'s "Bool"
  var value : Bool

  @public init(_ value: Bool) {
    self.value = value
  }
#endif

  /// Allow use in a Boolean context.
  @public func getLogicValue() -> Bool {
#if os(OSX) || (os(iOS) && (arch(i386) || arch(arm)))
    return value != 0
#else
    return value == true
#endif
  }

  /// Implicit conversion from C Boolean type to Swift Boolean type.
  @conversion @public func __conversion() -> Bool {
    return self.getLogicValue()
  }
}

extension ObjCBool : Reflectable {
	@public func getMirror() -> Mirror {
		return reflect(getLogicValue())
	}
}

extension Bool {
  /// Implicit conversion from Swift Boolean type to
  /// Objective-C Boolean type.
  @conversion @public func __conversion() -> ObjCBool {
#if os(OSX) || (os(iOS) && (arch(i386) || arch(arm)))
    return ObjCBool(self ? 1 : 0)
#else
    return ObjCBool(self ? Bool.true : Bool.false)
#endif
  }
}

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
  @public
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

