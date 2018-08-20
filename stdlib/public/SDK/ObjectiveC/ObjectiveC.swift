//===----------------------------------------------------------------------===//
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

@_exported
import ObjectiveC
import _SwiftObjectiveCOverlayShims

//===----------------------------------------------------------------------===//
// Objective-C Primitive Types
//===----------------------------------------------------------------------===//

/// The Objective-C BOOL type.
///
/// On 64-bit iOS, the Objective-C BOOL type is a typedef of C/C++
/// bool. Elsewhere, it is "signed char". The Clang importer imports it as
/// ObjCBool.
@_fixed_layout
public struct ObjCBool : ExpressibleByBooleanLiteral {
#if os(macOS) || (os(iOS) && (arch(i386) || arch(arm)))
  // On OS X and 32-bit iOS, Objective-C's BOOL type is a "signed char".
  var _value: Int8

  init(_ value: Int8) {
    self._value = value
  }

  public init(_ value: Bool) {
    self._value = value ? 1 : 0
  }

#else
  // Everywhere else it is C/C++'s "Bool"
  var _value: Bool

  public init(_ value: Bool) {
    self._value = value
  }
#endif

  /// The value of `self`, expressed as a `Bool`.
  public var boolValue: Bool {
#if os(macOS) || (os(iOS) && (arch(i386) || arch(arm)))
    return _value != 0
#else
    return _value
#endif
  }

  /// Create an instance initialized to `value`.
  @_transparent
  public init(booleanLiteral value: Bool) {
    self.init(value)
  }
}

extension ObjCBool : CustomReflectable {
  /// Returns a mirror that reflects `self`.
  public var customMirror: Mirror {
    return Mirror(reflecting: boolValue)
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
func _convertBoolToObjCBool(_ x: Bool) -> ObjCBool {
  return ObjCBool(x)
}

public // COMPILER_INTRINSIC
func _convertObjCBoolToBool(_ x: ObjCBool) -> Bool {
  return x.boolValue
}

/// The Objective-C SEL type.
///
/// The Objective-C SEL type is typically an opaque pointer. Swift
/// treats it as a distinct struct type, with operations to
/// convert between C strings and selectors.
///
/// The compiler has special knowledge of this type.
@_fixed_layout
public struct Selector : ExpressibleByStringLiteral {
  var ptr: OpaquePointer

  /// Create a selector from a string.
  public init(_ str : String) {
    ptr = str.withCString { sel_registerName($0).ptr }
  }

  // FIXME: Fast-path this in the compiler, so we don't end up with
  // the sel_registerName call at compile time.
  /// Create an instance initialized to `value`.
  public init(stringLiteral value: String) {
    self = sel_registerName(value)
  }
}

extension Selector : Equatable, Hashable {
  public static func ==(lhs: Selector, rhs: Selector) -> Bool {
    return sel_isEqual(lhs, rhs)
  }

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
    return String(_sel: self)
  }
}

extension String {
  /// Construct the C string representation of an Objective-C selector.
  public init(_sel: Selector) {
    // FIXME: This misses the ASCII optimization.
    self = String(cString: sel_getName(_sel))
  }
}

extension Selector : CustomReflectable {
  /// Returns a mirror that reflects `self`.
  public var customMirror: Mirror {
    return Mirror(reflecting: String(_sel: self))
  }
}

//===----------------------------------------------------------------------===//
// NSZone
//===----------------------------------------------------------------------===//

@_fixed_layout
public struct NSZone {
  var pointer: OpaquePointer
}

// Note: NSZone becomes Zone in Swift 3.
typealias Zone = NSZone

//===----------------------------------------------------------------------===//
// @autoreleasepool substitute
//===----------------------------------------------------------------------===//

public func autoreleasepool<Result>(
  invoking body: () throws -> Result
) rethrows -> Result {
  let pool = _swift_objc_autoreleasePoolPush()
  defer {
    _swift_objc_autoreleasePoolPop(pool)
  }
  return try body()
}

//===----------------------------------------------------------------------===//
// Mark YES and NO unavailable.
//===----------------------------------------------------------------------===//

@available(*, unavailable, message: "Use 'Bool' value 'true' instead")
public var YES: ObjCBool {
  fatalError("can't retrieve unavailable property")
}
@available(*, unavailable, message: "Use 'Bool' value 'false' instead")
public var NO: ObjCBool {
  fatalError("can't retrieve unavailable property")
}

//===----------------------------------------------------------------------===//
// NSObject
//===----------------------------------------------------------------------===//

// NSObject implements Equatable's == as -[NSObject isEqual:]
// NSObject implements Hashable's hashValue as -[NSObject hash]
// FIXME: what about NSObjectProtocol?

extension NSObject : Equatable, Hashable {
  /// Returns a Boolean value indicating whether two values are
  /// equal. `NSObject` implements this by calling `lhs.isEqual(rhs)`.
  ///
  /// Subclasses of `NSObject` can customize Equatable conformance by overriding
  /// `isEqual(_:)`. If two objects are equal, they must have the same hash
  /// value, so if you override `isEqual(_:)`, make sure you also override the
  /// `hash` property.
  ///
  /// - Parameters:
  ///   - lhs: A value to compare.
  ///   - rhs: Another value to compare.
  public static func == (lhs: NSObject, rhs: NSObject) -> Bool {
    return lhs.isEqual(rhs)
  }

  /// The hash value.
  ///
  /// `NSObject` implements this by returning `self.hash`. Subclasses can
  /// customize hashing by overriding the `hash` property.
  ///
  /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`
  ///
  /// - Note: the hash value is not guaranteed to be stable across
  ///   different invocations of the same program.  Do not persist the
  ///   hash value across program runs.
  @objc open // FIXME: Should be @nonobjc public. rdar://problem/42623458
  var hashValue: Int {
    return hash
  }

  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// NSObject implements this by feeding `self.hash` to the hasher. Subclasses
  /// can customize hashing by overriding the `hash` property.
  public func hash(into hasher: inout Hasher) {
    // FIXME: We should combine self.hash here, but hashValue is currently
    // overridable.
    hasher.combine(hashValue)
  }

  public func _rawHashValue(seed: (UInt64, UInt64)) -> Int {
    // FIXME: We should use self.hash here, but hashValue is currently
    // overridable.
    return self.hashValue._rawHashValue(seed: seed)
  }
}

extension NSObject : CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public var _cVarArgEncoding: [Int] {
    _autorelease(self)
    return _encodeBitsAsWords(self)
  }
}

