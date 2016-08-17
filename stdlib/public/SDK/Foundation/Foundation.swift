//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module
import CoreFoundation
import CoreGraphics

//===----------------------------------------------------------------------===//
// NSObject
//===----------------------------------------------------------------------===//

// These conformances should be located in the `ObjectiveC` module, but they can't
// be placed there because string bridging is not available there.
extension NSObject : CustomStringConvertible {}
extension NSObject : CustomDebugStringConvertible {}

//===----------------------------------------------------------------------===//
// Strings
//===----------------------------------------------------------------------===//

@available(*, unavailable, message: "Please use String or NSString")
public class NSSimpleCString {}

@available(*, unavailable, message: "Please use String or NSString")
public class NSConstantString {}

@_silgen_name("swift_convertStringToNSString")
public // COMPILER_INTRINSIC
func _convertStringToNSString(_ string: String) -> NSString {
  return string._bridgeToObjectiveC()
}

extension NSString : ExpressibleByStringLiteral {
  /// Create an instance initialized to `value`.
  public required convenience init(unicodeScalarLiteral value: StaticString) {
    self.init(stringLiteral: value)
  }

  public required convenience init(
    extendedGraphemeClusterLiteral value: StaticString
  ) {
    self.init(stringLiteral: value)
  }

  /// Create an instance initialized to `value`.
  public required convenience init(stringLiteral value: StaticString) {
    var immutableResult: NSString
    if value.hasPointerRepresentation {
      immutableResult = NSString(
        bytesNoCopy: UnsafeMutableRawPointer(mutating: value.utf8Start),
        length: Int(value.utf8CodeUnitCount),
        encoding: value.isASCII ? String.Encoding.ascii.rawValue : String.Encoding.utf8.rawValue,
        freeWhenDone: false)!
    } else {
      var uintValue = value.unicodeScalar
      immutableResult = NSString(
        bytes: &uintValue,
        length: 4,
        encoding: String.Encoding.utf32.rawValue)!
    }
    self.init(string: immutableResult as String)
  }
}

extension NSString : _HasCustomAnyHashableRepresentation {
  // Must be @nonobjc to prevent infinite recursion trying to bridge
  // AnyHashable to NSObject.
  @nonobjc
  public func _toCustomAnyHashable() -> AnyHashable? {
    // Consistently use Swift equality and hashing semantics for all strings.
    return AnyHashable(self as String)
  }
}

//===----------------------------------------------------------------------===//
// New Strings
//===----------------------------------------------------------------------===//

//
// Conversion from NSString to Swift's native representation
//

extension String {
  public init(_ cocoaString: NSString) {
    self = String(_cocoaString: cocoaString)
  }
}

extension String : _ObjectiveCBridgeable {
  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSString {
    // This method should not do anything extra except calling into the
    // implementation inside core.  (These two entry points should be
    // equivalent.)
    return unsafeBitCast(_bridgeToObjectiveCImpl() as AnyObject, to: NSString.self)
  }

  public static func _forceBridgeFromObjectiveC(
    _ x: NSString,
    result: inout String?
  ) {
    result = String(x)
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSString,
    result: inout String?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return result != nil
  }

  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSString?
  ) -> String {
    // `nil` has historically been used as a stand-in for an empty
    // string; map it to an empty string.
    if _slowPath(source == nil) { return String() }
    return String(source!)
  }
}

//===----------------------------------------------------------------------===//
// Numbers
//===----------------------------------------------------------------------===//

@_silgen_name("_swift_Foundation_TypePreservingNSNumberWithInt")
internal func _swift_Foundation_TypePreservingNSNumberWithInt(
  _ value: Int
) -> NSNumber

@_silgen_name("_swift_Foundation_TypePreservingNSNumberWithUInt")
internal func _swift_Foundation_TypePreservingNSNumberWithUInt(
  _ value: UInt
) -> NSNumber

@_silgen_name("_swift_Foundation_TypePreservingNSNumberWithFloat")
internal func _swift_Foundation_TypePreservingNSNumberWithFloat(
  _ value: Float
) -> NSNumber

@_silgen_name("_swift_Foundation_TypePreservingNSNumberWithDouble")
internal func _swift_Foundation_TypePreservingNSNumberWithDouble(
  _ value: Double
) -> NSNumber

@_silgen_name("_swift_Foundation_TypePreservingNSNumberWithCGFloat")
internal func _swift_Foundation_TypePreservingNSNumberWithCGFloat(
  _ value: CGFloat
) -> NSNumber

@_silgen_name("_swift_Foundation_TypePreservingNSNumberWithBool")
internal func _swift_Foundation_TypePreservingNSNumberWithBool(
  _ value: Bool
) -> NSNumber

@_silgen_name("_swift_Foundation_TypePreservingNSNumberGetKind")
internal func _swift_Foundation_TypePreservingNSNumberGetKind(
  _ value: NSNumber
) -> UInt32

// This enum has a matching counterpart in TypePreservingNSNumber.mm, please
// update both copies when changing it.
internal enum _SwiftTypePreservingNSNumberTag : Int {
  case SwiftInt = 1
  case SwiftUInt = 2
  case SwiftFloat = 3
  case SwiftDouble = 4
  case CoreGraphicsCGFloat = 5
  case SwiftBool = 6
}

@_silgen_name("_swift_Foundation_TypePreservingNSNumberGetAsInt")
internal func _swift_Foundation_TypePreservingNSNumberGetAsInt(
  _ value: NSNumber
) -> Int

@_silgen_name("_swift_Foundation_TypePreservingNSNumberGetAsUInt")
internal func _swift_Foundation_TypePreservingNSNumberGetAsUInt(
  _ value: NSNumber
) -> UInt

@_silgen_name("_swift_Foundation_TypePreservingNSNumberGetAsFloat")
internal func _swift_Foundation_TypePreservingNSNumberGetAsFloat(
  _ value: NSNumber
) -> Float

@_silgen_name("_swift_Foundation_TypePreservingNSNumberGetAsDouble")
internal func _swift_Foundation_TypePreservingNSNumberGetAsDouble(
  _ value: NSNumber
) -> Double

@_silgen_name("_swift_Foundation_TypePreservingNSNumberGetAsCGFloat")
internal func _swift_Foundation_TypePreservingNSNumberGetAsCGFloat(
  _ value: NSNumber
) -> CGFloat

@_silgen_name("_swift_Foundation_TypePreservingNSNumberGetAsBool")
internal func _swift_Foundation_TypePreservingNSNumberGetAsBool(
  _ value: NSNumber
) -> Bool

// Conversions between NSNumber and various numeric types. The
// conversion to NSNumber is automatic (auto-boxing), while conversion
// back to a specific numeric type requires a cast.
// FIXME: Incomplete list of types.
extension Int : _ObjectiveCBridgeable {
  public init(_ number: NSNumber) {
    self = number.intValue
  }

  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSNumber {
    return _swift_Foundation_TypePreservingNSNumberWithInt(self)
  }

  public static func _forceBridgeFromObjectiveC(
    _ x: NSNumber,
    result: inout Int?
  ) {
    result = x.intValue
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSNumber,
    result: inout Int?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }

  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSNumber?
  ) -> Int {
    return source!.intValue
  }
}

extension UInt : _ObjectiveCBridgeable {
  public init(_ number: NSNumber) {
    self = number.uintValue
  }

  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSNumber {
    return _swift_Foundation_TypePreservingNSNumberWithUInt(self)
  }

  public static func _forceBridgeFromObjectiveC(
    _ x: NSNumber,
    result: inout UInt?
  ) {
    result = x.uintValue
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSNumber,
    result: inout UInt?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }

  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSNumber?
  ) -> UInt {
    return source!.uintValue
  }
}

extension Float : _ObjectiveCBridgeable {
  public init(_ number: NSNumber) {
    self = number.floatValue
  }

  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSNumber {
    return _swift_Foundation_TypePreservingNSNumberWithFloat(self)
  }

  public static func _forceBridgeFromObjectiveC(
    _ x: NSNumber,
    result: inout Float?
  ) {
    result = x.floatValue
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSNumber,
    result: inout Float?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }

  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSNumber?
  ) -> Float {
    return source!.floatValue
  }
}

extension Double : _ObjectiveCBridgeable {
  public init(_ number: NSNumber) {
    self = number.doubleValue
  }

  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSNumber {
    return _swift_Foundation_TypePreservingNSNumberWithDouble(self)
  }

  public static func _forceBridgeFromObjectiveC(
    _ x: NSNumber,
    result: inout Double?
  ) {
    result = x.doubleValue
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSNumber,
    result: inout Double?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }

  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSNumber?
  ) -> Double {
    return source!.doubleValue
  }
}

extension Bool: _ObjectiveCBridgeable {
  public init(_ number: NSNumber) {
    self = number.boolValue
  }

  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSNumber {
    return _swift_Foundation_TypePreservingNSNumberWithBool(self)
  }

  public static func _forceBridgeFromObjectiveC(
    _ x: NSNumber,
    result: inout Bool?
  ) {
    result = x.boolValue
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSNumber,
    result: inout Bool?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }

  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSNumber?
  ) -> Bool {
    return source!.boolValue
  }
}

// CGFloat bridging.
extension CGFloat : _ObjectiveCBridgeable {
  public init(_ number: NSNumber) {
    self.native = CGFloat.NativeType(number)
  }

  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSNumber {
    return _swift_Foundation_TypePreservingNSNumberWithCGFloat(self)
  }

  public static func _forceBridgeFromObjectiveC(
    _ x: NSNumber,
    result: inout CGFloat?
  ) {
    var nativeResult: CGFloat.NativeType? = 0.0
    CGFloat.NativeType._forceBridgeFromObjectiveC(x, result: &nativeResult)
    result = CGFloat(nativeResult!)
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSNumber,
    result: inout CGFloat?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }

  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSNumber?
  ) -> CGFloat {
    return CGFloat(
             CGFloat.NativeType._unconditionallyBridgeFromObjectiveC(source))
  }
}

// Literal support for NSNumber
extension NSNumber
  : ExpressibleByFloatLiteral,
    ExpressibleByIntegerLiteral,
    ExpressibleByBooleanLiteral
{
  /// Create an instance initialized to `value`.
  public required convenience init(integerLiteral value: Int) {
    self.init(value: value)
  }

  /// Create an instance initialized to `value`.
  public required convenience init(floatLiteral value: Double) {
    self.init(value: value)
  }

  /// Create an instance initialized to `value`.
  public required convenience init(booleanLiteral value: Bool) {
    self.init(value: value)
  }
}

extension NSNumber : _HasCustomAnyHashableRepresentation {
  // Must be @nonobjc to prevent infinite recursion trying to bridge
  // AnyHashable to NSObject.
  @nonobjc
  public func _toCustomAnyHashable() -> AnyHashable? {
    guard let kind = _SwiftTypePreservingNSNumberTag(
      rawValue: Int(_swift_Foundation_TypePreservingNSNumberGetKind(self))
    ) else {
      if let nsDecimalNumber: NSDecimalNumber = self as? NSDecimalNumber {
        return AnyHashable(nsDecimalNumber as Decimal)
      }
      return nil
    }
    switch kind {
    case .SwiftInt:
      return AnyHashable(_swift_Foundation_TypePreservingNSNumberGetAsInt(self))
    case .SwiftUInt:
      return AnyHashable(_swift_Foundation_TypePreservingNSNumberGetAsUInt(self))
    case .SwiftFloat:
      return AnyHashable(_swift_Foundation_TypePreservingNSNumberGetAsFloat(self))
    case .SwiftDouble:
      return AnyHashable(_swift_Foundation_TypePreservingNSNumberGetAsDouble(self))
    case .CoreGraphicsCGFloat:
      return AnyHashable(_swift_Foundation_TypePreservingNSNumberGetAsCGFloat(self))
    case .SwiftBool:
      return AnyHashable(_swift_Foundation_TypePreservingNSNumberGetAsBool(self))
    }
  }
}

public let NSNotFound: Int = .max

//===----------------------------------------------------------------------===//
// Arrays
//===----------------------------------------------------------------------===//

extension NSArray : ExpressibleByArrayLiteral {
  /// Create an instance initialized with `elements`.
  public required convenience init(arrayLiteral elements: Any...) {
    // Let bridging take care of it.
    self.init(array: elements)
  }
}

extension Array : _ObjectiveCBridgeable {

  /// Private initializer used for bridging.
  ///
  /// The provided `NSArray` will be copied to ensure that the copy can
  /// not be mutated by other code.
  internal init(_cocoaArray: NSArray) {
    _sanityCheck(_isBridgedVerbatimToObjectiveC(Element.self),
      "Array can be backed by NSArray only when the element type can be bridged verbatim to Objective-C")
    // FIXME: We would like to call CFArrayCreateCopy() to avoid doing an
    // objc_msgSend() for instances of CoreFoundation types.  We can't do that
    // today because CFArrayCreateCopy() copies array contents unconditionally,
    // resulting in O(n) copies even for immutable arrays.
    //
    // <rdar://problem/19773555> CFArrayCreateCopy() is >10x slower than
    // -[NSArray copyWithZone:]
    //
    // The bug is fixed in: OS X 10.11.0, iOS 9.0, all versions of tvOS
    // and watchOS.
    self = Array(
      _immutableCocoaArray:
        unsafeBitCast(_cocoaArray.copy() as AnyObject, to: _NSArrayCore.self))
  }

  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSArray {
    return unsafeBitCast(self._bridgeToObjectiveCImpl(), to: NSArray.self)
  }

  public static func _forceBridgeFromObjectiveC(
    _ source: NSArray,
    result: inout Array?
  ) {
    _precondition(
      Swift._isBridgedToObjectiveC(Element.self),
      "array element type is not bridged to Objective-C")

    // If we have the appropriate native storage already, just adopt it.
    if let native =
        Array._bridgeFromObjectiveCAdoptingNativeStorageOf(source) {
      result = native
      return
    }

    if _fastPath(_isBridgedVerbatimToObjectiveC(Element.self)) {
      // Forced down-cast (possible deferred type-checking)
      result = Array(_cocoaArray: source)
      return
    }

    result = _arrayForceCast([AnyObject](_cocoaArray: source))
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ source: NSArray,
    result: inout Array?
  ) -> Bool {
    // Construct the result array by conditionally bridging each element.
    let anyObjectArr = [AnyObject](_cocoaArray: source)

    result = _arrayConditionalCast(anyObjectArr)
    return result != nil
  }

  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSArray?
  ) -> Array {
    // `nil` has historically been used as a stand-in for an empty
    // array; map it to an empty array instead of failing.
    if _slowPath(source == nil) { return Array() }

    // If we have the appropriate native storage already, just adopt it.
    if let native =
        Array._bridgeFromObjectiveCAdoptingNativeStorageOf(source!) {
      return native
    }

    if _fastPath(_isBridgedVerbatimToObjectiveC(Element.self)) {
      // Forced down-cast (possible deferred type-checking)
      return Array(_cocoaArray: source!)
    }

    return _arrayForceCast([AnyObject](_cocoaArray: source!))
  }
}

//===----------------------------------------------------------------------===//
// Dictionaries
//===----------------------------------------------------------------------===//

extension NSDictionary : ExpressibleByDictionaryLiteral {
  public required convenience init(
    dictionaryLiteral elements: (Any, Any)...
  ) {
    // FIXME: Unfortunate that the `NSCopying` check has to be done at runtime.
    self.init(
      objects: elements.map { $0.1 as AnyObject },
      forKeys: elements.map { $0.0 as AnyObject as! NSCopying },
      count: elements.count)
  }
}

extension Dictionary {
  /// Private initializer used for bridging.
  ///
  /// The provided `NSDictionary` will be copied to ensure that the copy can
  /// not be mutated by other code.
  public init(_cocoaDictionary: _NSDictionary) {
    _sanityCheck(
      _isBridgedVerbatimToObjectiveC(Key.self) &&
      _isBridgedVerbatimToObjectiveC(Value.self),
      "Dictionary can be backed by NSDictionary storage only when both key and value are bridged verbatim to Objective-C")
    // FIXME: We would like to call CFDictionaryCreateCopy() to avoid doing an
    // objc_msgSend() for instances of CoreFoundation types.  We can't do that
    // today because CFDictionaryCreateCopy() copies dictionary contents
    // unconditionally, resulting in O(n) copies even for immutable dictionaries.
    //
    // <rdar://problem/20690755> CFDictionaryCreateCopy() does not call copyWithZone:
    //
    // The bug is fixed in: OS X 10.11.0, iOS 9.0, all versions of tvOS
    // and watchOS.
    self = Dictionary(
      _immutableCocoaDictionary:
        unsafeBitCast(_cocoaDictionary.copy(with: nil) as AnyObject,
                      to: _NSDictionary.self))
  }
}

// Dictionary<Key, Value> is conditionally bridged to NSDictionary
extension Dictionary : _ObjectiveCBridgeable {
  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSDictionary {
    return unsafeBitCast(_bridgeToObjectiveCImpl() as AnyObject,
                         to: NSDictionary.self)
  }

  public static func _forceBridgeFromObjectiveC(
    _ d: NSDictionary,
    result: inout Dictionary?
  ) {
    if let native = [Key : Value]._bridgeFromObjectiveCAdoptingNativeStorageOf(
        d as AnyObject) {
      result = native
      return
    }

    if _isBridgedVerbatimToObjectiveC(Key.self) &&
       _isBridgedVerbatimToObjectiveC(Value.self) {
      result = [Key : Value](
        _cocoaDictionary: unsafeBitCast(d as AnyObject, to: _NSDictionary.self))
      return
    }

    // `Dictionary<Key, Value>` where either `Key` or `Value` is a value type
    // may not be backed by an NSDictionary.
    var builder = _DictionaryBuilder<Key, Value>(count: d.count)
    d.enumerateKeysAndObjects({
      (anyKey: Any, anyValue: Any,
       stop: UnsafeMutablePointer<ObjCBool>) in
      let anyObjectKey = anyKey as AnyObject
      let anyObjectValue = anyValue as AnyObject
      builder.add(
          key: Swift._forceBridgeFromObjectiveC(anyObjectKey, Key.self),
          value: Swift._forceBridgeFromObjectiveC(anyObjectValue, Value.self))
    })
    result = builder.take()
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSDictionary,
    result: inout Dictionary?
  ) -> Bool {
    let anyDict = x as [NSObject : AnyObject]
    if _isBridgedVerbatimToObjectiveC(Key.self) &&
       _isBridgedVerbatimToObjectiveC(Value.self) {
      result = Swift._dictionaryDownCastConditional(anyDict)
      return result != nil
    }

    result = Swift._dictionaryBridgeFromObjectiveCConditional(anyDict)
    return result != nil
  }

  public static func _unconditionallyBridgeFromObjectiveC(
    _ d: NSDictionary?
  ) -> Dictionary {
    // `nil` has historically been used as a stand-in for an empty
    // dictionary; map it to an empty dictionary.
    if _slowPath(d == nil) { return Dictionary() }

    if let native = [Key : Value]._bridgeFromObjectiveCAdoptingNativeStorageOf(
        d! as AnyObject) {
      return native
    }

    if _isBridgedVerbatimToObjectiveC(Key.self) &&
       _isBridgedVerbatimToObjectiveC(Value.self) {
      return [Key : Value](
        _cocoaDictionary: unsafeBitCast(d! as AnyObject, to: _NSDictionary.self))
    }

    // `Dictionary<Key, Value>` where either `Key` or `Value` is a value type
    // may not be backed by an NSDictionary.
    var builder = _DictionaryBuilder<Key, Value>(count: d!.count)
    d!.enumerateKeysAndObjects({
      (anyKey: Any, anyValue: Any,
       stop: UnsafeMutablePointer<ObjCBool>) in
      builder.add(
          key: Swift._forceBridgeFromObjectiveC(anyKey as AnyObject, Key.self),
          value: Swift._forceBridgeFromObjectiveC(anyValue as AnyObject, Value.self))
    })
    return builder.take()
  }
}

//===----------------------------------------------------------------------===//
// TextChecking
//===----------------------------------------------------------------------===//

extension NSTextCheckingResult.CheckingType {
    public static var allSystemTypes : NSTextCheckingResult.CheckingType {
        return NSTextCheckingResult.CheckingType(rawValue: 0xffffffff)
    }
    
    public static var allCustomTypes : NSTextCheckingResult.CheckingType {
        return NSTextCheckingResult.CheckingType(rawValue: 0xffffffff << 32)
    }
    
    public static var allTypes : NSTextCheckingResult.CheckingType {
        return NSTextCheckingResult.CheckingType(rawValue: UInt64.max)
    }
}

//===----------------------------------------------------------------------===//
// Fast enumeration
//===----------------------------------------------------------------------===//

// NB: This is a class because fast enumeration passes around interior pointers
// to the enumeration state, so the state cannot be moved in memory. We will
// probably need to implement fast enumeration in the compiler as a primitive
// to implement it both correctly and efficiently.
final public class NSFastEnumerationIterator : IteratorProtocol {
  var enumerable: NSFastEnumeration
  var state: [NSFastEnumerationState]
  var n: Int
  var count: Int

  /// Size of ObjectsBuffer, in ids.
  static var STACK_BUF_SIZE: Int { return 4 }

  var objects: [Unmanaged<AnyObject>?]

  public func next() -> Any? {
    if n == count {
      // FIXME: Is this check necessary before refresh()?
      if count == 0 { return nil }
      refresh()
      if count == 0 { return nil }
    }
    let next: Any = state[0].itemsPtr![n]!
    n += 1
    return next
  }

  func refresh() {
    _sanityCheck(objects.count > 0)
    n = 0
    objects.withUnsafeMutableBufferPointer {
      count = enumerable.countByEnumerating(
        with: &state,
        objects: AutoreleasingUnsafeMutablePointer($0.baseAddress!),
        count: $0.count)
    }
  }

  public init(_ enumerable: NSFastEnumeration) {
    self.enumerable = enumerable
    self.state = [ NSFastEnumerationState(
      state: 0, itemsPtr: nil,
      mutationsPtr: _fastEnumerationStorageMutationsPtr,
      extra: (0, 0, 0, 0, 0)) ]
    self.objects = Array(
      repeating: nil, count: NSFastEnumerationIterator.STACK_BUF_SIZE)
    self.n = -1
    self.count = -1
  }
}

extension NSArray : Sequence {
  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  final public func makeIterator() -> NSFastEnumerationIterator {
    return NSFastEnumerationIterator(self)
  }
}

/* TODO: API review
extension NSArray : Swift.Collection {
  final public var startIndex: Int {
    return 0
  }

  final public var endIndex: Int {
    return count
  }
}
 */

extension Set {
  /// Private initializer used for bridging.
  ///
  /// The provided `NSSet` will be copied to ensure that the copy can
  /// not be mutated by other code.
  public init(_cocoaSet: _NSSet) {
    _sanityCheck(_isBridgedVerbatimToObjectiveC(Element.self),
      "Set can be backed by NSSet _variantStorage only when the member type can be bridged verbatim to Objective-C")
    // FIXME: We would like to call CFSetCreateCopy() to avoid doing an
    // objc_msgSend() for instances of CoreFoundation types.  We can't do that
    // today because CFSetCreateCopy() copies dictionary contents
    // unconditionally, resulting in O(n) copies even for immutable dictionaries.
    //
    // <rdar://problem/20697680> CFSetCreateCopy() does not call copyWithZone:
    //
    // The bug is fixed in: OS X 10.11.0, iOS 9.0, all versions of tvOS
    // and watchOS.
    self = Set(
      _immutableCocoaSet:
        unsafeBitCast(_cocoaSet.copy(with: nil) as AnyObject, to: _NSSet.self))
  }
}

extension NSSet : Sequence {
  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func makeIterator() -> NSFastEnumerationIterator {
    return NSFastEnumerationIterator(self)
  }
}

extension NSOrderedSet : Sequence {
  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func makeIterator() -> NSFastEnumerationIterator {
    return NSFastEnumerationIterator(self)
  }
}

// FIXME: move inside NSIndexSet when the compiler supports this.
public struct NSIndexSetIterator : IteratorProtocol {
  public typealias Element = Int

  internal let _set: NSIndexSet
  internal var _first: Bool = true
  internal var _current: Int?

  internal init(set: NSIndexSet) {
    self._set = set
    self._current = nil
  }

  public mutating func next() -> Int? {
    if _first {
      _current = _set.firstIndex
      _first = false
    } else if let c = _current {
      _current = _set.indexGreaterThanIndex(c)
    } else {
      // current is already nil
    }
    if _current == NSNotFound {
      _current = nil
    }
    return _current
  }
}

extension NSIndexSet : Sequence {
  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func makeIterator() -> NSIndexSetIterator {
    return NSIndexSetIterator(set: self)
  }
}

// Set<Element> is conditionally bridged to NSSet
extension Set : _ObjectiveCBridgeable {
  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSSet {
    return unsafeBitCast(_bridgeToObjectiveCImpl() as AnyObject, to: NSSet.self)
  }

  public static func _forceBridgeFromObjectiveC(_ s: NSSet, result: inout Set?) {
    if let native =
      Set<Element>._bridgeFromObjectiveCAdoptingNativeStorageOf(s as AnyObject) {

      result = native
      return
    }

    if _isBridgedVerbatimToObjectiveC(Element.self) {
      result = Set<Element>(_cocoaSet: unsafeBitCast(s, to: _NSSet.self))
      return
    }

    // `Set<Element>` where `Element` is a value type may not be backed by
    // an NSSet.
    var builder = _SetBuilder<Element>(count: s.count)
    s.enumerateObjects({
      (anyMember: Any, stop: UnsafeMutablePointer<ObjCBool>) in
      builder.add(member: Swift._forceBridgeFromObjectiveC(
        anyMember as AnyObject, Element.self))
    })
    result = builder.take()
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSSet, result: inout Set?
  ) -> Bool {
    let anySet = x as Set<NSObject>
    if _isBridgedVerbatimToObjectiveC(Element.self) {
      result = Swift._setDownCastConditional(anySet)
      return result != nil
    }

    result = Swift._setBridgeFromObjectiveCConditional(anySet)
    return result != nil
  }

  public static func _unconditionallyBridgeFromObjectiveC(_ s: NSSet?) -> Set {
    // `nil` has historically been used as a stand-in for an empty
    // set; map it to an empty set.
    if _slowPath(s == nil) { return Set() }

    if let native =
      Set<Element>._bridgeFromObjectiveCAdoptingNativeStorageOf(s! as AnyObject) {

      return native
    }

    if _isBridgedVerbatimToObjectiveC(Element.self) {
      return Set<Element>(_cocoaSet: unsafeBitCast(s! as AnyObject,
                                                   to: _NSSet.self))
    }

    // `Set<Element>` where `Element` is a value type may not be backed by
    // an NSSet.
    var builder = _SetBuilder<Element>(count: s!.count)
    s!.enumerateObjects({
      (anyMember: Any, stop: UnsafeMutablePointer<ObjCBool>) in
      builder.add(member: Swift._forceBridgeFromObjectiveC(
        anyMember as AnyObject, Element.self))
    })
    return builder.take()
  }
}

/*
FIXME(id-as-any): uncomment this when we can cast NSSet to Set<AnyHashable>.
extension NSSet : _HasCustomAnyHashableRepresentation {
  // Must be @nonobjc to avoid infinite recursion during bridging
  @nonobjc
  public func _toCustomAnyHashable() -> AnyHashable? {
    var builder = _SetBuilder<Element>(count: s!.count)
    // FIXME(id-as-any): how to get the Hashable conformance here?
    s!.enumerateObjects({
      (anyMember: Any, stop: UnsafeMutablePointer<ObjCBool>) in
      builder.add(member: Swift._forceBridgeFromObjectiveC(
        anyMember as AnyObject, Element.self))
    })
    return AnyHashable(self as Set<AnyHashable>)
  }
}
*/

extension NSDictionary : Sequence {
  // FIXME: A class because we can't pass a struct with class fields through an
  // [objc] interface without prematurely destroying the references.
  final public class Iterator : IteratorProtocol {
    var _fastIterator: NSFastEnumerationIterator
    var _dictionary: NSDictionary {
      return _fastIterator.enumerable as! NSDictionary
    }

    public func next() -> (key: Any, value: Any)? {
      if let key = _fastIterator.next() {
        // Deliberately avoid the subscript operator in case the dictionary
        // contains non-copyable keys. This is rare since NSMutableDictionary
        // requires them, but we don't want to paint ourselves into a corner.
        return (key: key, value: _dictionary.object(forKey: key)!)
      }
      return nil
    }

    internal init(_ _dict: NSDictionary) {
      _fastIterator = NSFastEnumerationIterator(_dict)
    }
  }

  // Bridging subscript.
  @objc
  public subscript(key: Any) -> Any? {
    @objc(_swift_objectForKeyedSubscript:)
    get {
      // Deliberately avoid the subscript operator in case the dictionary
      // contains non-copyable keys. This is rare since NSMutableDictionary
      // requires them, but we don't want to paint ourselves into a corner.
      return self.object(forKey: key)
    }
  }

  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func makeIterator() -> Iterator {
    return Iterator(self)
  }
}

extension NSMutableDictionary {
  // Bridging subscript.
  override public subscript(key: Any) -> Any? {
    get {
      return self.object(forKey: key)
    }
    @objc(_swift_setObject:forKeyedSubscript:)
    set {
      // FIXME: Unfortunate that the `NSCopying` check has to be done at
      // runtime.
      let copyingKey = key as AnyObject as! NSCopying
      if let newValue = newValue {
        self.setObject(newValue, forKey: copyingKey)
      } else {
        self.removeObject(forKey: copyingKey)
      }
    }
  }
}

extension NSEnumerator : Sequence {
  /// Return an *iterator* over the *enumerator*.
  ///
  /// - Complexity: O(1).
  public func makeIterator() -> NSFastEnumerationIterator {
    return NSFastEnumerationIterator(self)
  }
}

//===----------------------------------------------------------------------===//
// Ranges
//===----------------------------------------------------------------------===//

extension NSRange {
  public init(_ x: Range<Int>) {
    location = x.lowerBound
    length = x.count
  }

  // FIXME(ABI)(compiler limitation): this API should be an extension on Range.
  // Can't express it now because the compiler does not support conditional
  // extensions with type equality constraints.
  public func toRange() -> Range<Int>? {
    if location == NSNotFound { return nil }
    return location..<(location+length)
  }
}

//===----------------------------------------------------------------------===//
// NSLocalizedString
//===----------------------------------------------------------------------===//

/// Returns a localized string, using the main bundle if one is not specified.
public
func NSLocalizedString(_ key: String,
                       tableName: String? = nil,
                       bundle: Bundle = Bundle.main,
                       value: String = "",
                       comment: String) -> String {
  return bundle.localizedString(forKey: key, value:value, table:tableName)
}

//===----------------------------------------------------------------------===//
// NSLog
//===----------------------------------------------------------------------===//

public func NSLog(_ format: String, _ args: CVarArg...) {
  withVaList(args) { NSLogv(format, $0) }
}

#if os(OSX)

//===----------------------------------------------------------------------===//
// NSRectEdge
//===----------------------------------------------------------------------===//

// In the SDK, the following NS*Edge constants are defined as macros for the
// corresponding CGRectEdge enumerators.  Thus, in the SDK, NS*Edge constants
// have CGRectEdge type.  This is not correct for Swift (as there is no
// implicit conversion to NSRectEdge).

@available(*, unavailable, renamed: "NSRectEdge.MinX")
public var NSMinXEdge: NSRectEdge {
  fatalError("unavailable property can't be accessed")
}
@available(*, unavailable, renamed: "NSRectEdge.MinY")
public var NSMinYEdge: NSRectEdge {
  fatalError("unavailable property can't be accessed")
}
@available(*, unavailable, renamed: "NSRectEdge.MaxX")
public var NSMaxXEdge: NSRectEdge {
  fatalError("unavailable property can't be accessed")
}
@available(*, unavailable, renamed: "NSRectEdge.MaxY")
public var NSMaxYEdge: NSRectEdge {
  fatalError("unavailable property can't be accessed")
}

extension NSRectEdge {
  public init(rectEdge: CGRectEdge) {
    self = NSRectEdge(rawValue: UInt(rectEdge.rawValue))!
  }
}

extension CGRectEdge {
  public init(rectEdge: NSRectEdge) {
    self = CGRectEdge(rawValue: UInt32(rectEdge.rawValue))!
  }
}

#endif

//===----------------------------------------------------------------------===//
// NSError (as an out parameter).
//===----------------------------------------------------------------------===//

public typealias NSErrorPointer = AutoreleasingUnsafeMutablePointer<NSError?>?

// Note: NSErrorPointer becomes ErrorPointer in Swift 3.
public typealias ErrorPointer = NSErrorPointer

public // COMPILER_INTRINSIC
let _nilObjCError: Error = _GenericObjCError.nilError

@_silgen_name("swift_convertNSErrorToError")
public // COMPILER_INTRINSIC
func _convertNSErrorToError(_ error: NSError?) -> Error {
  if let error = error {
    return error
  }
  return _nilObjCError
}

@_silgen_name("swift_convertErrorToNSError")
public // COMPILER_INTRINSIC
func _convertErrorToNSError(_ error: Error) -> NSError {
  return unsafeDowncast(_bridgeErrorToNSError(error), to: NSError.self)
}

//===----------------------------------------------------------------------===//
// Variadic initializers and methods
//===----------------------------------------------------------------------===//

extension NSPredicate {
  // + (NSPredicate *)predicateWithFormat:(NSString *)predicateFormat, ...;
  public
  convenience init(format predicateFormat: String, _ args: CVarArg...) {
    let va_args = getVaList(args)
    self.init(format: predicateFormat, arguments: va_args)
  }
}

extension NSExpression {
  // + (NSExpression *) expressionWithFormat:(NSString *)expressionFormat, ...;
  public
  convenience init(format expressionFormat: String, _ args: CVarArg...) {
    let va_args = getVaList(args)
    self.init(format: expressionFormat, arguments: va_args)
  }
}

extension NSString {
  public convenience init(format: NSString, _ args: CVarArg...) {
    // We can't use withVaList because 'self' cannot be captured by a closure
    // before it has been initialized.
    let va_args = getVaList(args)
    self.init(format: format as String, arguments: va_args)
  }

  public convenience init(
    format: NSString, locale: Locale?, _ args: CVarArg...
  ) {
    // We can't use withVaList because 'self' cannot be captured by a closure
    // before it has been initialized.
    let va_args = getVaList(args)
    self.init(format: format as String, locale: locale, arguments: va_args)
  }

  public class func localizedStringWithFormat(
    _ format: NSString, _ args: CVarArg...
  ) -> Self {
    return withVaList(args) {
      self.init(format: format as String, locale: Locale.current, arguments: $0)
    }
  }

  public func appendingFormat(_ format: NSString, _ args: CVarArg...)
  -> NSString {
    return withVaList(args) {
      self.appending(NSString(format: format as String, arguments: $0) as String) as NSString
    }
  }
}

extension NSMutableString {
  public func appendFormat(_ format: NSString, _ args: CVarArg...) {
    return withVaList(args) {
      self.append(NSString(format: format as String, arguments: $0) as String)
    }
  }
}

extension NSArray {
  // Overlay: - (instancetype)initWithObjects:(id)firstObj, ...
  public convenience init(objects elements: Any...) {
    self.init(array: elements)
  }
}

extension NSOrderedSet {
  // - (instancetype)initWithObjects:(id)firstObj, ...
  public convenience init(objects elements: Any...) {
    self.init(array: elements)
  }
}

extension NSSet {
  // - (instancetype)initWithObjects:(id)firstObj, ...
  public convenience init(objects elements: Any...) {
    self.init(array: elements)
  }
}

extension NSSet : ExpressibleByArrayLiteral {
  public required convenience init(arrayLiteral elements: Any...) {
    self.init(array: elements)
  }
}

extension NSOrderedSet : ExpressibleByArrayLiteral {
  public required convenience init(arrayLiteral elements: Any...) {
    self.init(array: elements)
  }
}

//===--- "Copy constructors" ----------------------------------------------===//
// These are needed to make Cocoa feel natural since we eliminated
// implicit bridging conversions from Objective-C to Swift
//===----------------------------------------------------------------------===//

extension NSArray {
  /// Initializes a newly allocated array by placing in it the objects
  /// contained in a given array.
  ///
  /// - Returns: An array initialized to contain the objects in
  ///    `anArray``. The returned object might be different than the
  ///    original receiver.
  ///
  /// Discussion: After an immutable array has been initialized in
  /// this way, it cannot be modified.
  @nonobjc
  public convenience init(array anArray: NSArray) {
    self.init(array: anArray as Array)
  }
}

extension NSString {
  /// Returns an `NSString` object initialized by copying the characters
  /// from another given string.
  ///
  /// - Returns: An `NSString` object initialized by copying the
  ///   characters from `aString`. The returned object may be different
  ///   from the original receiver.
  @nonobjc
  public convenience init(string aString: NSString) {
    self.init(string: aString as String)
  }
}

extension NSSet {
  /// Initializes a newly allocated set and adds to it objects from
  /// another given set.
  ///
  /// - Returns: An initialized objects set containing the objects from
  ///   `set`. The returned set might be different than the original
  ///   receiver.
  @nonobjc
  public convenience init(set anSet: NSSet) {
    // FIXME(performance)(compiler limitation): we actually want to do just
    // `self = anSet.copy()`, but Swift does not have factory
    // initializers right now.
    let numElems = anSet.count
    let stride = MemoryLayout<Optional<UnsafeRawPointer>>.stride
    let alignment = MemoryLayout<Optional<UnsafeRawPointer>>.alignment
    let bufferSize = stride * numElems
    assert(stride == MemoryLayout<AnyObject>.stride)
    assert(alignment == MemoryLayout<AnyObject>.alignment)

    let rawBuffer = UnsafeMutableRawPointer.allocate(
      bytes: bufferSize, alignedTo: alignment)
    defer {
      rawBuffer.deallocate(bytes: bufferSize, alignedTo: alignment)
      _fixLifetime(anSet)
    }
    let valueBuffer = rawBuffer.bindMemory(
     to: Optional<UnsafeRawPointer>.self, capacity: numElems)

    CFSetGetValues(anSet, valueBuffer)
    let valueBufferForInit = rawBuffer.assumingMemoryBound(to: AnyObject.self)
    self.init(objects: valueBufferForInit, count: numElems)
  }
}

extension NSDictionary {
  /// Initializes a newly allocated dictionary and adds to it objects from
  /// another given dictionary.
  ///
  /// - Returns: An initialized dictionary—which might be different
  ///   than the original receiver—containing the keys and values
  ///   found in `otherDictionary`.
  @objc(_swiftInitWithDictionary_NSDictionary:)
  public convenience init(dictionary otherDictionary: NSDictionary) {
    // FIXME(performance)(compiler limitation): we actually want to do just
    // `self = otherDictionary.copy()`, but Swift does not have factory
    // initializers right now.
    let numElems = otherDictionary.count
    let stride = MemoryLayout<AnyObject>.stride
    let alignment = MemoryLayout<AnyObject>.alignment
    let singleSize = stride * numElems
    let totalSize = singleSize * 2
    _sanityCheck(stride == MemoryLayout<NSCopying>.stride)
    _sanityCheck(alignment == MemoryLayout<NSCopying>.alignment)

    // Allocate a buffer containing both the keys and values.
    let buffer = UnsafeMutableRawPointer.allocate(
      bytes: totalSize, alignedTo: alignment)
    defer {
      buffer.deallocate(bytes: totalSize, alignedTo: alignment)
      _fixLifetime(otherDictionary)
    }

    let valueBuffer = buffer.bindMemory(to: AnyObject.self, capacity: numElems)
    let buffer2 = buffer + singleSize
    let keyBuffer = buffer2.bindMemory(to: AnyObject.self, capacity: numElems)

    _stdlib_NSDictionary_getObjects(
      nsDictionary: otherDictionary,
      objects: valueBuffer,
      andKeys: keyBuffer)

    let keyBufferCopying = buffer2.assumingMemoryBound(to: NSCopying.self)
    self.init(objects: valueBuffer, forKeys: keyBufferCopying, count: numElems)
  }
}

@_silgen_name("__NSDictionaryGetObjects")
func _stdlib_NSDictionary_getObjects(
  nsDictionary: NSDictionary,
  objects: UnsafeMutablePointer<AnyObject>?,
  andKeys keys: UnsafeMutablePointer<AnyObject>?
)


//===----------------------------------------------------------------------===//
// NSUndoManager
//===----------------------------------------------------------------------===//

@_silgen_name("NS_Swift_NSUndoManager_registerUndoWithTargetHandler")
internal func NS_Swift_NSUndoManager_registerUndoWithTargetHandler(
  _ self_: AnyObject,
  _ target: AnyObject,
  _ handler: @escaping @convention(block) (AnyObject) -> Void)

extension UndoManager {
  @available(*, unavailable, renamed: "registerUndo(withTarget:handler:)")
  public func registerUndoWithTarget<TargetType : AnyObject>(_ target: TargetType, handler: (TargetType) -> Void) {
    fatalError("This API has been renamed")
  }

  @available(OSX 10.11, iOS 9.0, *)
  public func registerUndo<TargetType : AnyObject>(withTarget target: TargetType, handler: @escaping (TargetType) -> Void) {
    // The generic blocks use a different ABI, so we need to wrap the provided
    // handler in something ObjC compatible.
    let objcCompatibleHandler: (AnyObject) -> Void = { internalTarget in
      handler(internalTarget as! TargetType)
    }
    NS_Swift_NSUndoManager_registerUndoWithTargetHandler(
      self as AnyObject, target as AnyObject, objcCompatibleHandler)
  }
}

//===----------------------------------------------------------------------===//
// NSCoder
//===----------------------------------------------------------------------===//

@_silgen_name("NS_Swift_NSCoder_decodeObject")
internal func NS_Swift_NSCoder_decodeObject(
  _ self_: AnyObject,
  _ error: NSErrorPointer) -> AnyObject?

@_silgen_name("NS_Swift_NSCoder_decodeObjectForKey")
internal func NS_Swift_NSCoder_decodeObjectForKey(
  _ self_: AnyObject,
  _ key: AnyObject,
  _ error: NSErrorPointer) -> AnyObject?

@_silgen_name("NS_Swift_NSCoder_decodeObjectOfClassForKey")
internal func NS_Swift_NSCoder_decodeObjectOfClassForKey(
  _ self_: AnyObject,
  _ cls: AnyObject,
  _ key: AnyObject,
  _ error: NSErrorPointer) -> AnyObject?

@_silgen_name("NS_Swift_NSCoder_decodeObjectOfClassesForKey")
internal func NS_Swift_NSCoder_decodeObjectOfClassesForKey(
  _ self_: AnyObject,
  _ classes: NSSet?,
  _ key: AnyObject,
  _ error: NSErrorPointer) -> AnyObject?


@available(OSX 10.11, iOS 9.0, *)
internal func resolveError(_ error: NSError?) throws {
  if let error = error, error.code != NSCoderValueNotFoundError {
    throw error
  }
}

extension NSCoder {
  @available(*, unavailable, renamed: "decodeObject(of:forKey:)")
  public func decodeObjectOfClass<DecodedObjectType>(
    _ cls: DecodedObjectType.Type, forKey key: String
  ) -> DecodedObjectType?
    where DecodedObjectType : NSCoding, DecodedObjectType : NSObject {
    fatalError("This API has been renamed")
  }

  public func decodeObject<DecodedObjectType>(
    of cls: DecodedObjectType.Type, forKey key: String
  ) -> DecodedObjectType?
    where DecodedObjectType : NSCoding, DecodedObjectType : NSObject {
    let result = NS_Swift_NSCoder_decodeObjectOfClassForKey(self as AnyObject, cls as AnyObject, key as AnyObject, nil)
    return result as? DecodedObjectType
  }

  @available(*, unavailable, renamed: "decodeObject(of:forKey:)")
  @nonobjc
  public func decodeObjectOfClasses(_ classes: NSSet?, forKey key: String) -> AnyObject? {
    fatalError("This API has been renamed")
  }

  @nonobjc
  public func decodeObject(of classes: [AnyClass]?, forKey key: String) -> Any? {
    var classesAsNSObjects: NSSet? = nil
    if let theClasses = classes {
      classesAsNSObjects = NSSet(array: theClasses.map { $0 as AnyObject })
    }
    return NS_Swift_NSCoder_decodeObjectOfClassesForKey(self as AnyObject, classesAsNSObjects, key as AnyObject, nil).map { $0 as Any }
  }

  @nonobjc
  @available(OSX 10.11, iOS 9.0, *)
  public func decodeTopLevelObject() throws -> Any? {
    var error: NSError?
    let result = NS_Swift_NSCoder_decodeObject(self as AnyObject, &error)
    try resolveError(error)
    return result.map { $0 as Any }
  }

  @available(*, unavailable, renamed: "decodeTopLevelObject(forKey:)")
  public func decodeTopLevelObjectForKey(_ key: String) throws -> AnyObject? {
    fatalError("This API has been renamed")
  }

  @nonobjc
  @available(OSX 10.11, iOS 9.0, *)
  public func decodeTopLevelObject(forKey key: String) throws -> AnyObject? {
    var error: NSError?
    let result = NS_Swift_NSCoder_decodeObjectForKey(self as AnyObject, key as AnyObject, &error)
    try resolveError(error)
    return result
  }

  @available(*, unavailable, renamed: "decodeTopLevelObject(of:forKey:)")
  public func decodeTopLevelObjectOfClass<DecodedObjectType>(
    _ cls: DecodedObjectType.Type, forKey key: String
  ) throws -> DecodedObjectType?
    where DecodedObjectType : NSCoding, DecodedObjectType : NSObject {
    fatalError("This API has been renamed")
  }

  @available(OSX 10.11, iOS 9.0, *)
  public func decodeTopLevelObject<DecodedObjectType>(
    of cls: DecodedObjectType.Type, forKey key: String
  ) throws -> DecodedObjectType?
    where DecodedObjectType : NSCoding, DecodedObjectType : NSObject {
    var error: NSError?
    let result = NS_Swift_NSCoder_decodeObjectOfClassForKey(self as AnyObject, cls as AnyObject, key as AnyObject, &error)
    try resolveError(error)
    return result as? DecodedObjectType
  }

  @nonobjc
  @available(*, unavailable, renamed: "decodeTopLevelObject(of:forKey:)")
  public func decodeTopLevelObjectOfClasses(_ classes: NSSet?, forKey key: String) throws -> AnyObject? {
    fatalError("This API has been renamed")
  }

  @nonobjc
  @available(OSX 10.11, iOS 9.0, *)
  public func decodeTopLevelObject(of classes: [AnyClass]?, forKey key: String) throws -> Any? {
    var error: NSError? = nil
    var classesAsNSObjects: NSSet? = nil
    if let theClasses = classes {
      classesAsNSObjects = NSSet(array: theClasses.map { $0 as AnyObject })
    }
    let result = NS_Swift_NSCoder_decodeObjectOfClassesForKey(self as AnyObject, classesAsNSObjects, key as AnyObject, &error)
    try resolveError(error)
    return result.map { $0 as Any }
  }
}

//===----------------------------------------------------------------------===//
// NSKeyedUnarchiver
//===----------------------------------------------------------------------===//

@_silgen_name("NS_Swift_NSKeyedUnarchiver_unarchiveObjectWithData")
internal func NS_Swift_NSKeyedUnarchiver_unarchiveObjectWithData(
  _ self_: AnyObject,
  _ data: AnyObject,
  _ error: NSErrorPointer) -> AnyObject?

extension NSKeyedUnarchiver {
  @available(OSX 10.11, iOS 9.0, *)
  @nonobjc
  public class func unarchiveTopLevelObjectWithData(_ data: NSData) throws -> AnyObject? {
    var error: NSError?
    let result = NS_Swift_NSKeyedUnarchiver_unarchiveObjectWithData(self, data as AnyObject, &error)
    try resolveError(error)
    return result
  }
}

//===----------------------------------------------------------------------===//
// Mirror/Quick Look Conformance
//===----------------------------------------------------------------------===//

extension NSURL : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    guard let str = absoluteString else { return .text("Unknown URL") }
    return .url(str)
  }
}

extension NSRange : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(self, children: ["location": location, "length": length])
  }
}

extension NSRange : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .range(Int64(location), Int64(length))
  }
}

extension NSDate : CustomPlaygroundQuickLookable {
  var summary: String {
    let df = DateFormatter()
    df.dateStyle = .medium
    df.timeStyle = .short
    return df.string(from: self as Date)
  }

  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(summary)
  }
}

extension NSSet : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(reflecting: self as Set<NSObject>)
  }
}

extension NSString : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(self as String)
  }
}

extension NSArray : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(reflecting: self as [AnyObject])
  }
}

extension NSDictionary : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(reflecting: self as [NSObject : AnyObject])
  }
}

@available(*, deprecated, renamed:"NSCoding", message: "Please use NSCoding")
typealias Coding = NSCoding

@available(*, deprecated, renamed:"NSCoder", message: "Please use NSCoder")
typealias Coder = NSCoder

@available(*, deprecated, renamed:"NSKeyedUnarchiver", message: "Please use NSKeyedUnarchiver")
typealias KeyedUnarchiver = NSKeyedUnarchiver

@available(*, deprecated, renamed:"NSKeyedArchiver", message: "Please use NSKeyedArchiver")
typealias KeyedArchiver = NSKeyedArchiver

//===----------------------------------------------------------------------===//
// AnyHashable
//===----------------------------------------------------------------------===//

extension AnyHashable : _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> NSObject {
    // This is unprincipled, but pretty much any object we'll encounter in
    // Swift is NSObject-conforming enough to have -hash and -isEqual:.
    return unsafeBitCast(base as AnyObject, to: NSObject.self)
  }

  public static func _forceBridgeFromObjectiveC(
    _ x: NSObject,
    result: inout AnyHashable?
  ) {
    result = AnyHashable(x)
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSObject,
    result: inout AnyHashable?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return result != nil
  }

  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSObject?
  ) -> AnyHashable {
    // `nil` has historically been used as a stand-in for an empty
    // string; map it to an empty string.
    if _slowPath(source == nil) { return AnyHashable(String()) }
    return AnyHashable(source!)
  }
}

//===----------------------------------------------------------------------===//
// CVarArg for bridged types
//===----------------------------------------------------------------------===//

extension CVarArg where Self: _ObjectiveCBridgeable {
  /// Default implementation for bridgeable types.
  public var _cVarArgEncoding: [Int] {
    let object = self._bridgeToObjectiveC()
    _autorelease(object)
    return _encodeBitsAsWords(object)
  }
}

extension String: CVarArg {}
extension Array: CVarArg {}
extension Dictionary: CVarArg {}
extension Set: CVarArg {}
