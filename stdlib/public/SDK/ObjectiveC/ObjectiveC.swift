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
/// On 64-bit iOS, the Objective-C BOOL type is a typedef of C/C++
/// bool. Elsewhere, it is "signed char". The Clang importer imports it as
/// ObjCBool.
public struct ObjCBool : BooleanType, BooleanLiteralConvertible {
#if os(OSX) || (os(iOS) && (arch(i386) || arch(arm)))
  // On OS X and 32-bit iOS, Objective-C's BOOL type is a "signed char".
  var value: Int8

  init(_ value: Int8) {
    self.value = value
  }

  public init(_ value: Bool) {
    self.value = value ? 1 : 0
  }

#else
  // Everywhere else it is C/C++'s "Bool"
  var value : Bool

  public init(_ value: Bool) {
    self.value = value
  }
#endif

  /// The value of `self`, expressed as a `Bool`.
  public var boolValue: Bool {
#if os(OSX) || (os(iOS) && (arch(i386) || arch(arm)))
    return value != 0
#else
    return value
#endif
  }

  /// Create an instance initialized to `value`.
  @transparent
  public init(booleanLiteral value: Bool) {
    self.init(value)
  }
}

extension ObjCBool : _Reflectable {
  /// Returns a mirror that reflects `self`.
  public func _getMirror() -> MirrorType {
    return _reflect(boolValue)
  }
}

extension ObjCBool : CustomStringConvertible {
  /// A textual representation of `self`.
  public var description: String {
    return self.boolValue.description
  }
}

// Functions used to implicitly bridge ObjCBool types to Swift's Bool type.

public // COMPILER_INTRINSIC
func _convertBoolToObjCBool(x: Bool) -> ObjCBool {
  return ObjCBool(x)
}
public // COMPILER_INTRINSIC
func _convertObjCBoolToBool(x: ObjCBool) -> Bool {
  return Bool(x)
}

/// The Objective-C SEL type.
///
/// The Objective-C SEL type is typically an opaque pointer. Swift
/// treats it as a distinct struct type, with operations to
/// convert between C strings and selectors.
///
/// The compiler has special knowledge of this type.
public struct Selector : StringLiteralConvertible, NilLiteralConvertible {
  var ptr : COpaquePointer

  /// Create a selector from a string.
  public init(_ str : String) {
    ptr = str.withCString { sel_registerName($0).ptr }
  }

  /// Create an instance initialized to `value`.
  public init(unicodeScalarLiteral value: String) {
    self.init(value)
  }

  /// Construct a selector from `value`.
  public init(extendedGraphemeClusterLiteral value: String) {
    self.init(value)
  }

  // FIXME: Fast-path this in the compiler, so we don't end up with
  // the sel_registerName call at compile time.
  /// Create an instance initialized to `value`.
  public init(stringLiteral value: String) {
    self = sel_registerName(value)
  }

  public init() {
    ptr = nil
  }
  
  /// Create an instance initialized with `nil`.
  @transparent public
  init(nilLiteral: ()) {
    ptr = nil
  }
}

public func ==(lhs: Selector, rhs: Selector) -> Bool {
  return sel_isEqual(lhs, rhs)
}

extension Selector : Equatable, Hashable {
  /// The hash value.
  ///
  /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`
  ///
  /// - Note: the hash value is not guaranteed to be stable across
  ///   different invocations of the same program.  Do not persist the
  ///   hash value across program runs.
  public var hashValue: Int {
    return ptr.hashValue
  }
}

extension Selector : CustomStringConvertible {
  /// A textual representation of `self`.
  public var description: String {
    if let s = String.fromCStringRepairingIllFormedUTF8(sel_getName(self)).0 {
      return s
    }
    return "<NULL>"
  }
}

extension String {
  /// Construct the C string representation of an Objective-C selector.
  public init(_sel: Selector) {
    // FIXME: This misses the ASCII optimization.
    self = String.fromCString(sel_getName(_sel))!
  }
}

extension Selector : _Reflectable {
  /// Returns a mirror that reflects `self`.
  public func _getMirror() -> MirrorType {
    return _reflect(String(_sel: self))
  }
}

//===----------------------------------------------------------------------===//
// NSZone
//===----------------------------------------------------------------------===//

public struct NSZone : NilLiteralConvertible {
  var pointer : COpaquePointer

  public init() { pointer = nil }

  /// Create an instance initialized with `nil`.
  @transparent public
  init(nilLiteral: ()) {
    pointer = nil
  }
}

//===----------------------------------------------------------------------===//
// FIXME: @autoreleasepool substitute
//===----------------------------------------------------------------------===//
@asmname("objc_autoreleasePoolPush") 
func __pushAutoreleasePool() -> COpaquePointer
@asmname("objc_autoreleasePoolPop") 
func __popAutoreleasePool(pool: COpaquePointer)

public func autoreleasepool(@noescape code: () -> ()) {
  let pool = __pushAutoreleasePool()
  code()
  __popAutoreleasePool(pool)
}

//===----------------------------------------------------------------------===//
// Mark YES and NO unavailable.
//===----------------------------------------------------------------------===//

@available(*, unavailable, message="Use 'Bool' value 'true' instead") public
let YES = ObjCBool(true)
@available(*, unavailable, message="Use 'Bool' value 'false' instead") public
let NO = ObjCBool(false)

// FIXME: We can't make the fully-generic versions @transparent due to
// rdar://problem/19418937, so here are some @transparent overloads
// for ObjCBool
@transparent
public func && <T : BooleanType>(
  lhs: T, @autoclosure rhs: () -> ObjCBool
) -> Bool {
  return lhs.boolValue ? rhs().boolValue : false
}

@transparent
public func || <T : BooleanType>(
  lhs: T, @autoclosure rhs: () -> ObjCBool
) -> Bool {
  return lhs.boolValue ? true : rhs().boolValue
}

//===----------------------------------------------------------------------===//
// NSObject
//===----------------------------------------------------------------------===//

// NSObject implements Equatable's == as -[NSObject isEqual:]
// NSObject implements Hashable's hashValue() as -[NSObject hash]
// FIXME: what about NSObjectProtocol?

extension NSObject : Equatable, Hashable {
  /// The hash value.
  ///
  /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`
  ///
  /// - Note: the hash value is not guaranteed to be stable across
  ///   different invocations of the same program.  Do not persist the
  ///   hash value across program runs.
  public var hashValue: Int {
    return hash
  }
}

public func == (lhs: NSObject, rhs: NSObject) -> Bool {
  return lhs.isEqual(rhs)
}

extension NSObject : CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public var _cVarArgEncoding: [Word] {
    _autorelease(self)
    return _encodeBitsAsWords(self)
  }
}

