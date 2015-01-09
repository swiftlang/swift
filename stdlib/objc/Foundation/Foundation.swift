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

@exported import Foundation // Clang module
import CoreFoundation
import CoreGraphics

//===----------------------------------------------------------------------===//
// Enums
//===----------------------------------------------------------------------===//

// FIXME: one day this will be bridged from CoreFoundation and we
// should drop it here. <rdar://problem/14497260> (need support
// for CF bridging)
public let kCFStringEncodingASCII: CFStringEncoding = 0x0600

// FIXME: <rdar://problem/16074941> NSStringEncoding doesn't work on 32-bit
public typealias NSStringEncoding = UInt
public let NSASCIIStringEncoding: UInt = 1
public let NSNEXTSTEPStringEncoding: UInt = 2
public let NSJapaneseEUCStringEncoding: UInt = 3
public let NSUTF8StringEncoding: UInt = 4
public let NSISOLatin1StringEncoding: UInt = 5
public let NSSymbolStringEncoding: UInt = 6
public let NSNonLossyASCIIStringEncoding: UInt = 7
public let NSShiftJISStringEncoding: UInt = 8
public let NSISOLatin2StringEncoding: UInt = 9
public let NSUnicodeStringEncoding: UInt = 10
public let NSWindowsCP1251StringEncoding: UInt = 11
public let NSWindowsCP1252StringEncoding: UInt = 12
public let NSWindowsCP1253StringEncoding: UInt = 13
public let NSWindowsCP1254StringEncoding: UInt = 14
public let NSWindowsCP1250StringEncoding: UInt = 15
public let NSISO2022JPStringEncoding: UInt = 21
public let NSMacOSRomanStringEncoding: UInt = 30
public let NSUTF16StringEncoding: UInt = NSUnicodeStringEncoding
public let NSUTF16BigEndianStringEncoding: UInt = 0x90000100
public let NSUTF16LittleEndianStringEncoding: UInt = 0x94000100
public let NSUTF32StringEncoding: UInt = 0x8c000100
public let NSUTF32BigEndianStringEncoding: UInt = 0x98000100
public let NSUTF32LittleEndianStringEncoding: UInt = 0x9c000100


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
  /// **Note:** the hash value is not guaranteed to be stable across
  /// different invocations of the same program.  Do not persist the
  /// hash value across program runs.
  public var hashValue: Int {
    return hash
  }
}

public func == (lhs: NSObject, rhs: NSObject) -> Bool {
  return lhs.isEqual(rhs)
}

extension NSObject : Printable {}

//===----------------------------------------------------------------------===//
// Strings
//===----------------------------------------------------------------------===//

@availability(*, unavailable, message="Please use String or NSString") public
class NSSimpleCString {}

@availability(*, unavailable, message="Please use String or NSString") public
class NSConstantString {}

@asmname("swift_convertStringToNSString")
public // COMPILER_INTRINSIC
func _convertStringToNSString(string: String) -> NSString {
  return string._bridgeToObjectiveC()
}

public // COMPILER_INTRINSIC
func _convertNSStringToString(nsstring: NSString) -> String {
  var result: String?
  String._forceBridgeFromObjectiveC(nsstring, result: &result)
  return result!
}

extension NSString : StringLiteralConvertible {
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
        bytesNoCopy: UnsafeMutablePointer<Void>(value.utf8Start),
        length: Int(value.byteSize),
        encoding: value.isASCII ? NSASCIIStringEncoding : NSUTF8StringEncoding,
        freeWhenDone: false)!
    } else {
      var uintValue = value.unicodeScalar
      immutableResult = NSString(
        bytes: &uintValue,
        length: 4,
        encoding: NSUTF32StringEncoding)!
    }
    self.init(string: immutableResult)
  }
}

//===----------------------------------------------------------------------===//
// New Strings
//===----------------------------------------------------------------------===//
extension NSString : _CocoaStringType {}

/// Sets variables in Swift's core stdlib that allow it to
/// bridge Cocoa strings properly.  Currently invoked by a HACK in
/// Misc.mm; a better mechanism may be needed.
@asmname("__swift_initializeCocoaStringBridge") 
public // COMPILER_INTRINSIC
func __swift_initializeCocoaStringBridge() -> COpaquePointer {
  _cocoaStringToContiguous = _cocoaStringToContiguousImpl
  _cocoaStringReadAll = _cocoaStringReadAllImpl
  _cocoaStringLength = _cocoaStringLengthImpl
  _cocoaStringSlice = _cocoaStringSliceImpl
  _cocoaStringSubscript = _cocoaStringSubscriptImpl
  return COpaquePointer()
}

// When used as a _CocoaStringType, an NSString should be either
// immutable or uniquely-referenced, and not have a buffer of
// contiguous UTF-16.  Ideally these distinctions would be captured in
// the type system, so one would have to explicitly convert NSString
// to a type that conforms.  Unfortunately, we don't have a way to do
// that without an allocation (to wrap NSString in another class
// instance) or growing these protocol instances from one word to four
// (by way of allowing structs to conform).  Fortunately, correctness
// doesn't depend on these preconditions but efficiency might,
// because producing a _StringBuffer from an
// NSString-as-_CocoaStringType is assumed to require allocation and
// buffer copying.
//
func _cocoaStringReadAllImpl(
  source: _CocoaStringType, destination: UnsafeMutablePointer<UTF16.CodeUnit>) {
  let cfSelf = unsafeBitCast(source, CFString.self)
  CFStringGetCharacters(
  cfSelf, CFRange(location: 0, length: CFStringGetLength(cfSelf)), destination)
}
  
func _cocoaStringToContiguousImpl(
  source: _CocoaStringType, range: Range<Int>, minimumCapacity: Int
) -> _StringBuffer {
  let cfSelf = unsafeBitCast(source, CFString.self)
  _sanityCheck(CFStringGetCharactersPtr(cfSelf) == nil,
    "Known contiguously-stored strings should already be converted to Swift")

  var startIndex = range.startIndex
  var count = range.endIndex - startIndex

  var buffer = _StringBuffer(capacity: max(count, minimumCapacity), 
                             initialSize: count, elementWidth: 2)

  CFStringGetCharacters(
    cfSelf, CFRange(location: startIndex, length: count), 
    UnsafeMutablePointer<UniChar>(buffer.start))
  
  return buffer
}

func _cocoaStringLengthImpl(source: _CocoaStringType) -> Int {
  // FIXME: Not ultra-fast, but reliable... but we're counting on an
  // early demise for this function anyhow
  return (source as! NSString).length
}

func _cocoaStringSliceImpl(
  target: _StringCore, subRange: Range<Int>) -> _StringCore {
  
  let buffer = _NSOpaqueString(
    owner: String(target), 
    subRange:
      NSRange(location: subRange.startIndex, length: count(subRange)))
  
  return _StringCore(
    baseAddress: nil,
    count: count(subRange),
    elementShift: 1,
    hasCocoaBuffer: true,
    owner: buffer)
}

func _cocoaStringSubscriptImpl(
  target: _StringCore, position: Int) -> UTF16.CodeUnit {
  let cfSelf = unsafeBitCast(target.cocoaBuffer!, CFString.self)
  _sanityCheck(CFStringGetCharactersPtr(cfSelf)._isNull,
    "Known contiguously-stored strings should already be converted to Swift")

  return CFStringGetCharacterAtIndex(cfSelf, position)
}

//
// NSString slice subclasses created when converting String to
// NSString.  We take care to avoid creating long wrapper chains
// when converting back and forth.
//

// A substring of an arbitrary immutable NSString.  The underlying
// NSString is assumed to not provide contiguous UTF-16 storage.
final class _NSOpaqueString : NSString {
  override var length: Int { 
    return subRange.length
  }

  override func characterAtIndex(index: Int) -> unichar {
    return owner.characterAtIndex(index + subRange.location)
  }

  override func getCharacters(buffer: UnsafeMutablePointer<unichar>,
                              range aRange: NSRange) {

    owner.getCharacters(
      buffer, 
      range: NSRange(location: aRange.location + subRange.location, 
                     length: aRange.length))
  }
  
  //
  // Implement sub-slicing without adding layers of wrapping
  // 
  override func substringFromIndex(start: Int) -> String {
    return _NSOpaqueString(
             owner: owner, 
             subRange: NSRange(location: subRange.location + start, 
                               length: subRange.length - start))
  }

  override func substringToIndex(end: Int) -> String {
    return _NSOpaqueString(
             owner: owner, 
             subRange: NSRange(location: subRange.location, length: end))
  }

  override func substringWithRange(aRange: NSRange) -> String {
    return _NSOpaqueString(
             owner: owner, 
             subRange: NSRange(location: aRange.location + subRange.location, 
                               length: aRange.length))
  }

  override init() {
    _sanityCheckFailure("init() not implemented for _NSOpaqueString")
  }

  init(owner: String, subRange: NSRange) {
    self.owner = owner
    self.subRange = subRange
    super.init()
  }

  required init(coder aDecoder: NSCoder) {
    _sanityCheckFailure("init(coder:) not implemented for _NSOpaqueString")
  }

  //
  // Implement copy; since this string is immutable we can just return ourselves
  override func copy() -> AnyObject {
    return self
  }
  
  var owner: NSString
  var subRange: NSRange
}


//
// Conversion from NSString to Swift's native representation
//

extension String {
  public init(_ cocoaString: NSString) {
    if let wrapped = (cocoaString as AnyObject) as? _NSContiguousString {
      self._core = wrapped._core
      return
    }

    // Treat it as a CF object because presumably that's what these
    // things tend to be, and CF has a fast path that avoids
    // objc_msgSend
    let cfValue = unsafeBitCast(cocoaString, CFString.self)

    // "copy" it into a value to be sure nobody will modify behind
    // our backs.  In practice, when value is already immutable, this
    // just does a retain.
    let cfImmutableValue: CFString = CFStringCreateCopy(nil, cfValue)

    let length = CFStringGetLength(cfImmutableValue)

    // Look first for null-terminated ASCII
    // Note: the code in clownfish appears to guarantee
    // nul-termination, but I'm waiting for an answer from Chris Kane
    // about whether we can count on it for all time or not.
    let nulTerminatedASCII = CFStringGetCStringPtr(
      cfImmutableValue, kCFStringEncodingASCII)

    // start will hold the base pointer of contiguous storage, if it
    // is found.
    var start = UnsafeMutablePointer<RawByte>(nulTerminatedASCII)
    let isUTF16 = nulTerminatedASCII._isNull
    if (isUTF16) {
      start = UnsafeMutablePointer(CFStringGetCharactersPtr(cfImmutableValue))
    }

    self._core = _StringCore(
      baseAddress: COpaquePointer(start),
      count: length,
      elementShift: isUTF16 ? 1 : 0,
      hasCocoaBuffer: true,
      owner: unsafeBitCast(cfImmutableValue, Optional<AnyObject>.self))
  }
}

extension String : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public static func _getObjectiveCType() -> Any.Type {
    return NSString.self
  }

  public func _bridgeToObjectiveC() -> NSString {
    // This method should not do anything extra except calling into the
    // implementation inside core.  (These two entry points should be
    // equivalent.)
    return unsafeBitCast(_bridgeToObjectiveCImpl(), NSString.self)
  }

  public static func _forceBridgeFromObjectiveC(
    x: NSString,
    inout result: String?
  ) {
    result = String(x)
  }
  
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSString,
    inout result: String?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return result != nil
  }
}

//===----------------------------------------------------------------------===//
// Numbers
//===----------------------------------------------------------------------===//

// Conversions between NSNumber and various numeric types. The
// conversion to NSNumber is automatic (auto-boxing), while conversion
// back to a specific numeric type requires a cast.
// FIXME: Incomplete list of types.
extension Int : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public init(_ number: NSNumber) {
    value = number.integerValue.value
  }

  public static func _getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }

  public func _bridgeToObjectiveC() -> NSNumber {
    return NSNumber(integer: self)
  }

  public static func _forceBridgeFromObjectiveC(
    x: NSNumber,
    inout result: Int?
  ) {
    result = x.integerValue
  }
  
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSNumber,
    inout result: Int?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }
}

extension UInt : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public init(_ number: NSNumber) {
    value = number.unsignedIntegerValue.value
  }

  public static func _getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }

  public func _bridgeToObjectiveC() -> NSNumber {
    // FIXME: Need a blacklist for certain methods that should not
    // import NSUInteger as Int.
    return NSNumber(unsignedInteger: Int(self.value))
  }

  public static func _forceBridgeFromObjectiveC(
    x: NSNumber,
    inout result: UInt?
  ) {
    result = UInt(x.unsignedIntegerValue.value)
  }
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSNumber,
    inout result: UInt?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }
}

extension Float : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public init(_ number: NSNumber) {
    self = number.floatValue
  }

  public static func _getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }

  public func _bridgeToObjectiveC() -> NSNumber {
    return NSNumber(float: self)
  }

  public static func _forceBridgeFromObjectiveC(
    x: NSNumber,
    inout result: Float?
  ) {
    result = x.floatValue
  }
  
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSNumber,
    inout result: Float?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }
}

extension Double : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public init(_ number: NSNumber) {
    self = number.doubleValue
  }

  public static func _getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }

  public func _bridgeToObjectiveC() -> NSNumber {
    return NSNumber(double: self)
  }

  public static func _forceBridgeFromObjectiveC(
    x: NSNumber,
    inout result: Double?
  ) {
    result = x.doubleValue
  }
  
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSNumber,
    inout result: Double?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }
}

extension Bool: _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public init(_ number: NSNumber) {
    if number.boolValue { self = true }
    else { self = false }
  }

  public static func _getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }

  public func _bridgeToObjectiveC() -> NSNumber {
    return NSNumber(bool: self)
  }

  public static func _forceBridgeFromObjectiveC(
    x: NSNumber,
    inout result: Bool?
  ) {
    result = x.boolValue
  }

  public static func _conditionallyBridgeFromObjectiveC(
    x: NSNumber,
    inout result: Bool?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }
}

// CGFloat bridging.
extension CGFloat : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public init(_ number: NSNumber) {
    self.native = CGFloat.NativeType(number)
  }

  public static func _getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }

  public func _bridgeToObjectiveC() -> NSNumber {
    return self.native._bridgeToObjectiveC()
  }

  public static func _forceBridgeFromObjectiveC(
    x: NSNumber,
    inout result: CGFloat?
  ) {
    var nativeResult: CGFloat.NativeType? = 0.0
    CGFloat.NativeType._forceBridgeFromObjectiveC(x, result: &nativeResult)
    result = CGFloat(nativeResult!)
  }

  public static func _conditionallyBridgeFromObjectiveC(
    x: NSNumber,
    inout result: CGFloat?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }
}

// Literal support for NSNumber
extension NSNumber : FloatLiteralConvertible, IntegerLiteralConvertible,
                     BooleanLiteralConvertible {
  /// Create an instance initialized to `value`.
  public required convenience init(integerLiteral value: Int) {
    self.init(integer: value)
  }

  /// Create an instance initialized to `value`.
  public required convenience init(floatLiteral value: Double) {
    self.init(double: value)
  }

  /// Create an instance initialized to `value`.
  public required convenience init(booleanLiteral value: Bool) {
    self.init(bool: value)
  }
}

public let NSNotFound: Int = .max

//===----------------------------------------------------------------------===//
// Arrays
//===----------------------------------------------------------------------===//

extension NSArray : ArrayLiteralConvertible {
  /// Create an instance initialized with `elements`.
  public required convenience init(arrayLiteral elements: AnyObject...) {
    // + (instancetype)arrayWithObjects:(const id [])objects count:(NSUInteger)cnt;
    let x = _extractOrCopyToNativeArrayBuffer(elements._buffer)
    self.init(
      objects: UnsafeMutablePointer(x.baseAddress), count: x.count)
    _fixLifetime(x)
  }
}

/// The entry point for converting `NSArray` to `Array` in bridge
/// thunks.  Used, for example, to expose ::
///
///   func f([NSView]) {}
///
/// to Objective-C code as a method that accepts an `NSArray`.  This operation
/// is referred to as a "forced conversion" in ../../../docs/Arrays.rst
public func _convertNSArrayToArray<T>(source: NSArray) -> [T] {
  var result: [T]?
  Array._forceBridgeFromObjectiveC(source, result: &result)
  return result!
}

/// The entry point for converting `Array` to `NSArray` in bridge
/// thunks.  Used, for example, to expose ::
///
///   func f() -> [NSView] { return [] }
///
/// to Objective-C code as a method that returns an `NSArray`.
public func _convertArrayToNSArray<T>(arr: [T]) -> NSArray {
  return arr._bridgeToObjectiveC()
}

extension Array : _ObjectiveCBridgeable {

  /// Construct from the given `NSArray`.  If `noCopy` is `true`,
  /// either `source` must be known to be immutable, or the resulting
  /// `Array` must not survive across code that could mutate `source`.
  internal init(_fromNSArray source: NSArray, noCopy: Bool = false) {
    // _NSArrayCoreType has selectors compatible with those
    // of NSArray; we use it to decouple the core stdlib from Foundation.
    // Bit-cast our NSArray to _NSArrayCoreType so it can be
    // adopted by an _ArrayBuffer.
    let cocoa = unsafeBitCast(source, _NSArrayCoreType.self)
    self = Array(_fromCocoaArray: cocoa, noCopy: noCopy)
  }

  public static func _isBridgedToObjectiveC() -> Bool {
    return Swift._isBridgedToObjectiveC(T.self)
  }

  public static func _getObjectiveCType() -> Any.Type {
    return NSArray.self
  }

  public func _bridgeToObjectiveC() -> NSArray {
    return unsafeBitCast(self._buffer._asCocoaArray(), NSArray.self)
  }

  public static func _forceBridgeFromObjectiveC(
    source: NSArray,
    inout result: Array?
  ) {
    _precondition(
      Swift._isBridgedToObjectiveC(T.self),
      "array element type is not bridged to Objective-C")

    // If we have the appropriate native storage already, just adopt it.
    if let native = Array._bridgeFromObjectiveCAdoptingNativeStorage(source) {
      result = native
      return
    }
    
    if _fastPath(_isBridgedVerbatimToObjectiveC(T.self)) {
      // Forced down-cast (possible deferred type-checking)
      result = Array(_fromNSArray: source)
      return
    }

    result = _arrayForceCast([AnyObject](_fromNSArray: source))
  }

  public static func _conditionallyBridgeFromObjectiveC(
    source: NSArray,
    inout result: Array?
  ) -> Bool {
    // Construct the result array by conditionally bridging each element.
    var anyObjectArr = [AnyObject](_fromNSArray: source)
    
    result = _arrayConditionalCast(anyObjectArr)
    return result != nil
  }
}

//===----------------------------------------------------------------------===//
// Dictionaries
//===----------------------------------------------------------------------===//

extension NSDictionary : DictionaryLiteralConvertible {
  public required convenience init(
    dictionaryLiteral elements: (NSCopying, AnyObject)...
  ) {
    self.init(
      objects: elements.map { (AnyObject?)($0.1) },
      forKeys: elements.map { (NSCopying?)($0.0) },
      count: elements.count)
  }
}

extension Dictionary {
  /// Private initializer used for bridging.
  ///
  /// The provided `NSDictionary` will be copied to ensure that the copy can
  /// not be mutated by other code.
  public init(_cocoaDictionary: _NSDictionaryType) {
    let cfValue = unsafeBitCast(_cocoaDictionary, CFDictionary.self)
    let copy = CFDictionaryCreateCopy(nil, cfValue)
    self = Dictionary(
      _immutableCocoaDictionary:
        unsafeBitCast(copy, _NSDictionaryType.self))
  }
}

/// The entry point for bridging `NSDictionary` to `Dictionary` in bridge
/// thunks.  Used, for example, to expose ::
///
///   func f([String : String]) {}
///
/// to Objective-C code as a method that accepts an `NSDictionary`.
///
/// This is a forced downcast.  This operation should have O(1) complexity
/// when `Key` and `Value` are bridged verbatim.
///
/// The cast can fail if bridging fails.  The actual checks and bridging can be
/// deferred.
public func _convertNSDictionaryToDictionary<
    Key : Hashable, Value>(d: NSDictionary)
    -> [Key : Value] {
  // Note: there should be *a good justification* for doing something else
  // than just dispatching to `_forceBridgeFromObjectiveC`.
  var result: [Key : Value]?
  Dictionary._forceBridgeFromObjectiveC(d, result: &result)
  return result!
}

// FIXME: right now the following is O(n), not O(1).

/// The entry point for bridging `Dictionary` to `NSDictionary` in bridge
/// thunks.  Used, for example, to expose ::
///
///   func f() -> [String : String] {}
///
/// to Objective-C code as a method that returns an `NSDictionary`.
///
/// This is a forced downcast.  This operation should have O(1) complexity.
///
/// The cast can fail if bridging fails.  The actual checks and bridging can be
/// deferred.
public func _convertDictionaryToNSDictionary<Key, Value>(
    d: [Key : Value]
) -> NSDictionary {
  
  // Note: there should be *a good justification* for doing something else
  // than just dispatching to `_bridgeToObjectiveC`.
  return d._bridgeToObjectiveC()
}

// Dictionary<Key, Value> is conditionally bridged to NSDictionary
extension Dictionary : _ObjectiveCBridgeable {
  public static func _getObjectiveCType() -> Any.Type {
    return NSDictionary.self
  }

  public func _bridgeToObjectiveC() -> NSDictionary {
    return unsafeBitCast(_bridgeToObjectiveCImpl(), NSDictionary.self)
  }

  public static func _forceBridgeFromObjectiveC(
    d: NSDictionary,
    inout result: Dictionary?
  ) {
    if let native = [Key : Value]._bridgeFromObjectiveCAdoptingNativeStorage(
        d as AnyObject) {
      result = native
      return
    }

    if _isBridgedVerbatimToObjectiveC(Key.self) &&
       _isBridgedVerbatimToObjectiveC(Value.self) {
      result = [Key : Value](
        _cocoaDictionary: unsafeBitCast(d, _NSDictionaryType.self))
      return
    }

    // `Dictionary<Key, Value>` where either `Key` or `Value` is a value type
    // may not be backed by an NSDictionary.
    var builder = _DictionaryBuilder<Key, Value>(count: d.count)
    d.enumerateKeysAndObjectsUsingBlock {
      (anyObjectKey: AnyObject!, anyObjectValue: AnyObject!,
       stop: UnsafeMutablePointer<ObjCBool>) in
      builder.add(
          key: Swift._forceBridgeFromObjectiveC(anyObjectKey, Key.self),
          value: Swift._forceBridgeFromObjectiveC(anyObjectValue, Value.self))
    }
    result = builder.take()
  }

  public static func _conditionallyBridgeFromObjectiveC(
    x: NSDictionary,
    inout result: Dictionary?
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

  public static func _isBridgedToObjectiveC() -> Bool {
    return Swift._isBridgedToObjectiveC(Key.self) &&
           Swift._isBridgedToObjectiveC(Value.self)
  }
}

//===----------------------------------------------------------------------===//
// General objects
//===----------------------------------------------------------------------===//

extension NSObject : CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    _autorelease(self)
    return _encodeBitsAsWords(self)
  }
}

//===----------------------------------------------------------------------===//
// Fast enumeration
//===----------------------------------------------------------------------===//


// NB: This is a class because fast enumeration passes around interior pointers
// to the enumeration state, so the state cannot be moved in memory. We will
// probably need to implement fast enumeration in the compiler as a primitive
// to implement it both correctly and efficiently.
final public class NSFastGenerator : GeneratorType {
  var enumerable: NSFastEnumeration
  var state: [NSFastEnumerationState]
  var n: Int
  var count: Int

  /// Size of ObjectsBuffer, in ids.
  var STACK_BUF_SIZE: Int { return 4 }

  /// Must have enough space for STACK_BUF_SIZE object references.
  struct ObjectsBuffer {
    var buf = (COpaquePointer(), COpaquePointer(),
               COpaquePointer(), COpaquePointer())
  }
  var objects: [ObjectsBuffer]

  public func next() -> AnyObject? {
    if n == count {
      // FIXME: Is this check necessary before refresh()?
      if count == 0 { return .None }
      refresh()
      if count == 0 { return .None }
    }
    var next : AnyObject = state[0].itemsPtr[n]!
    ++n
    return next
  }

  func refresh() {
    n = 0
    count = enumerable.countByEnumeratingWithState(
      state._baseAddressIfContiguous,
      objects: AutoreleasingUnsafeMutablePointer(
        objects._baseAddressIfContiguous),
      count: STACK_BUF_SIZE)
  }

  public init(_ enumerable: NSFastEnumeration) {
    self.enumerable = enumerable
    self.state = [NSFastEnumerationState](count: 1, repeatedValue: NSFastEnumerationState())
    self.state[0].state = 0
    self.objects = [ObjectsBuffer](count: 1, repeatedValue: ObjectsBuffer())
    self.n = -1
    self.count = -1
  }
}

extension NSArray : SequenceType {
  final public
  
  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// Complexity: O(1)
  func generate() -> NSFastGenerator {
    return NSFastGenerator(self)
  }
}

/*
// FIXME: <rdar://problem/16951124> prevents this from being used
extension NSArray : Swift.CollectionType {
  final
  var startIndex: Int {
    return 0
  }
  
  final
  var endIndex: Int {
    return count
  }

  subscript(i: Int) -> AnyObject {
    return self.objectAtIndex(i)
  }
}
*/

extension Set {
  /// Private initializer used for bridging.
  ///
  /// The provided `NSSet` will be copied to ensure that the copy can
  /// not be mutated by other code.
  public init(_cocoaSet: _NSSetType) {
    let cfValue = unsafeBitCast(_cocoaSet, CFSet.self)
    let copy = CFSetCreateCopy(nil, cfValue)
    self = Set(_immutableCocoaSet: unsafeBitCast(copy, _NSSetType.self))
  }
}

extension NSSet : SequenceType {
  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// Complexity: O(1)
  public func generate() -> NSFastGenerator {
    return NSFastGenerator(self)
  }
}

extension NSOrderedSet : SequenceType {
  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// Complexity: O(1)
  public func generate() -> NSFastGenerator {
    return NSFastGenerator(self)
  }
}

// FIXME: move inside NSIndexSet when the compiler supports this.
public struct NSIndexSetGenerator : GeneratorType {
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

extension NSIndexSet : SequenceType {
  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// Complexity: O(1)
  public func generate() -> NSIndexSetGenerator {
    return NSIndexSetGenerator(set: self)
  }
}

// FIXME: right now the following is O(n), not O(1).

/// The entry point for bridging `Set` to `NSSet` in bridge
/// thunks.  Used, for example, to expose ::
///
///   func f() -> Set<String> {}
///
/// to Objective-C code as a method that returns an `NSSet`.
///
/// This is a forced downcast.  This operation should have O(1) complexity.
///
/// The cast can fail if bridging fails.  The actual checks and bridging can be
/// deferred.
public func _convertSetToNSSet<T>(s: Set<T>) -> NSSet {
  return s._bridgeToObjectiveC()
}

/// The entry point for bridging `NSSet` to `Set` in bridge
/// thunks.  Used, for example, to expose ::
///
///   func f(Set<String>) {}
///
/// to Objective-C code as a method that accepts an `NSSet`.
///
/// This is a forced downcast.  This operation should have O(1) complexity
/// when `T` is bridged verbatim.
///
/// The cast can fail if bridging fails.  The actual checks and bridging can be
/// deferred.
public func _convertNSSetToSet<T: Hashable>(s: NSSet) -> Set<T> {
  var result: Set<T>?
  Set._forceBridgeFromObjectiveC(s, result: &result)
  return result!
}

// Set<T> is conditionally bridged to NSSet
extension Set : _ObjectiveCBridgeable {
  public static func _getObjectiveCType() -> Any.Type {
    return NSSet.self
  }

  public func _bridgeToObjectiveC() -> NSSet {
    return unsafeBitCast(_bridgeToObjectiveCImpl(), NSSet.self)
  }

  public static func _forceBridgeFromObjectiveC(s: NSSet, inout result: Set?) {
    if let native = Set<T>._bridgeFromObjectiveCAdoptingNativeStorage(s as AnyObject) {
      result = native
      return
    }

    if _isBridgedVerbatimToObjectiveC(T.self) {
      result = Set<T>(_cocoaSet: unsafeBitCast(s, _NSSetType.self))
      return
    }

    // `Set<T>` where `T` is a value type may not be backed by an NSSet.
    var builder = _SetBuilder<T>(count: s.count)
    s.enumerateObjectsUsingBlock {
      (anyObjectMember: AnyObject!, stop: UnsafeMutablePointer<ObjCBool>) in
      builder.add(member: Swift._forceBridgeFromObjectiveC(anyObjectMember, T.self))
    }
    result = builder.take()
  }

  public static func _conditionallyBridgeFromObjectiveC(x: NSSet, inout result: Set?) -> Bool {
    let anySet = x as Set<NSObject>
    if _isBridgedVerbatimToObjectiveC(T.self) {
      result = Swift._setDownCastConditional(anySet)
      return result != nil
    }

    result = Swift._setBridgeFromObjectiveCConditional(anySet)
    return result != nil
  }

  public static func _isBridgedToObjectiveC() -> Bool {
    return Swift._isBridgedToObjectiveC(T.self)
  }
}

extension NSDictionary : SequenceType {
  // FIXME: A class because we can't pass a struct with class fields through an
  // [objc] interface without prematurely destroying the references.
  final public class Generator : GeneratorType {
    var _fastGenerator: NSFastGenerator
    var _dictionary: NSDictionary {
      return _fastGenerator.enumerable as! NSDictionary
    }

    public func next() -> (key: AnyObject, value: AnyObject)? {
      switch _fastGenerator.next() {
      case .None:
        return .None
      case .Some(var key):
        // Deliberately avoid the subscript operator in case the dictionary
        // contains non-copyable keys. This is rare since NSMutableDictionary
        // requires them, but we don't want to paint ourselves into a corner.
        return (key: key, value: _dictionary.objectForKey(key)!)
      }
    }

    init(_ _dict: NSDictionary) {
      _fastGenerator = NSFastGenerator(_dict)
    }
  }

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// Complexity: O(1)
  public func generate() -> Generator {
    return Generator(self)
  }
}

extension NSEnumerator : SequenceType {
  /// Return a *generator* over the *enumerator*.
  ///
  /// Complexity: O(1)
  public func generate() -> NSFastGenerator {
    return NSFastGenerator(self)
  }
}

//===----------------------------------------------------------------------===//
// Ranges
//===----------------------------------------------------------------------===//

extension NSRange {
  public init(_ x: Range<Int>) {
    location = x.startIndex
    length = count(x)
  }

  public func toRange() -> Range<Int>? {
    if location == NSNotFound { return nil }
    return Range(start: location, end: location + length)
  }
}

//===----------------------------------------------------------------------===//
// NSLocalizedString
//===----------------------------------------------------------------------===//

/// Returns a localized string, using the main bundle if one is not specified.
public 
func NSLocalizedString(key: String,
                       tableName: String? = nil,
                       bundle: NSBundle = NSBundle.mainBundle(),
                       value: String = "",
                       #comment: String) -> String {
  return bundle.localizedStringForKey(key, value:value, table:tableName)
}

//===----------------------------------------------------------------------===//
// Reflection
//===----------------------------------------------------------------------===//

@asmname("swift_ObjCMirror_count") 
func _getObjCCount(_MagicMirrorData) -> Int
@asmname("swift_ObjCMirror_subscript") 
func _getObjCChild(Int, _MagicMirrorData) -> (String, MirrorType)

func _getObjCSummary(data: _MagicMirrorData) -> String {
  // FIXME: Trying to call debugDescription on AnyObject crashes.
  // <rdar://problem/16349526>
  // Work around by unsafeBitCast'ing to NSObject and hoping for the best.
  return (data._loadValue() as NSObject).debugDescription
}

struct _ObjCMirror: MirrorType {
  let data: _MagicMirrorData

  var value: Any { return data.objcValue }
  var valueType: Any.Type { return data.objcValueType }
  var objectIdentifier: ObjectIdentifier? {
    return data._loadValue() as ObjectIdentifier
  }
  var count: Int {
    return _getObjCCount(data)
  }
  subscript(i: Int) -> (String, MirrorType) {
    return _getObjCChild(i, data)
  }
  var summary: String {
    return _getObjCSummary(data)
  }
  var quickLookObject: QuickLookObject? {
    return _getClassQuickLookObject(data)
  }
  var disposition: MirrorDisposition { return .ObjCObject }
}

struct _ObjCSuperMirror: MirrorType {
  let data: _MagicMirrorData

  var value: Any { return data.objcValue }
  var valueType: Any.Type { return data.objcValueType }

  // Suppress the value identifier for super mirrors.
  var objectIdentifier: ObjectIdentifier? {
    return nil
  }
  var count: Int {
    return _getObjCCount(data)
  }
  subscript(i: Int) -> (String, MirrorType) {
    return _getObjCChild(i, data)
  }
  var summary: String {
    return _getObjCSummary(data)
  }
  var quickLookObject: QuickLookObject? {
    return _getClassQuickLookObject(data)
  }
  var disposition: MirrorDisposition { return .ObjCObject }
}

//===----------------------------------------------------------------------===//
// NSLog
//===----------------------------------------------------------------------===//

public func NSLog(format: String, args: CVarArgType...) {
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

public var NSMinXEdge: NSRectEdge {
  return NSRectEdge(CGRectEdge.MinXEdge.rawValue)
}
public var NSMinYEdge: NSRectEdge {
  return NSRectEdge(CGRectEdge.MinYEdge.rawValue)
}
public var NSMaxXEdge: NSRectEdge {
  return NSRectEdge(CGRectEdge.MaxXEdge.rawValue)
}
public var NSMaxYEdge: NSRectEdge {
  return NSRectEdge(CGRectEdge.MaxYEdge.rawValue)
}

#endif

//===----------------------------------------------------------------------===//
// NSError (as an out parameter).
//===----------------------------------------------------------------------===//

public typealias NSErrorPointer = AutoreleasingUnsafeMutablePointer<NSError?>

//===----------------------------------------------------------------------===//
// Variadic initializers and methods
//===----------------------------------------------------------------------===//

extension NSPredicate {
  // + (NSPredicate *)predicateWithFormat:(NSString *)predicateFormat, ...;
  public
  convenience init?(format predicateFormat: String, _ args: CVarArgType...) {
    let va_args = getVaList(args)
    self.init(format: predicateFormat, arguments: va_args)
  }
}

extension NSExpression {
  // + (NSExpression *) expressionWithFormat:(NSString *)expressionFormat, ...;
  public
  convenience init(format expressionFormat: String, _ args: CVarArgType...) {
    let va_args = getVaList(args)
    self.init(format: expressionFormat, arguments: va_args)
  }
}

extension NSString {
  public
  convenience init(format: NSString, _ args: CVarArgType...) {
    // We can't use withVaList because 'self' cannot be captured by a closure
    // before it has been initialized.
    let va_args = getVaList(args)
    self.init(format: format, arguments: va_args)
  }
  
  public
  convenience init(
    format: NSString, locale: NSLocale?, _ args: CVarArgType...
  ) {
    // We can't use withVaList because 'self' cannot be captured by a closure
    // before it has been initialized.
    let va_args = getVaList(args)
    self.init(format: format, locale: locale, arguments: va_args)
  }

  public
  class func localizedStringWithFormat(
    format: NSString, _ args: CVarArgType...
  ) -> Self {
    return withVaList(args) {
      self(format: format, locale: NSLocale.currentLocale(), arguments: $0)
    }
  }

  public
  func stringByAppendingFormat(format: NSString, _ args: CVarArgType...)
  -> NSString {
    return withVaList(args) {
      self.stringByAppendingString(NSString(format: format, arguments: $0))
    }
  }
}

extension NSMutableString {
  public
  func appendFormat(format: NSString, _ args: CVarArgType...) {
    return withVaList(args) {
      self.appendString(NSString(format: format, arguments: $0))
    }
  }
}

extension NSArray {
  // Overlay: - (instancetype)initWithObjects:(id)firstObj, ...
  public
  convenience init(objects elements: AnyObject...) {
    // - (instancetype)initWithObjects:(const id [])objects count:(NSUInteger)cnt;
    let x = _extractOrCopyToNativeArrayBuffer(elements._buffer)
    // Use Imported:
    // @objc(initWithObjects:count:)
    //    init(withObjects objects: UnsafePointer<AnyObject?>,
    //    count cnt: Int)
    self.init(objects: UnsafeMutablePointer(x.baseAddress), count: x.count)
    _fixLifetime(x)
  }
}

extension NSDictionary {
  // - (instancetype)initWithObjectsAndKeys:(id)firstObject, ...
  public
  convenience init(objectsAndKeys objects: AnyObject...) {
    // - (instancetype)initWithObjects:(NSArray *)objects forKeys:(NSArray *)keys;
    var values: [AnyObject] = []
    var keys:   [AnyObject] = []
    for var i = 0; i < objects.count; i += 2 {
      values.append(objects[i])
      keys.append(objects[i+1])
    }
    // - (instancetype)initWithObjects:(NSArray *)values forKeys:(NSArray *)keys;
    self.init(objects: values, forKeys: keys)
  }
}

extension NSOrderedSet {
  // - (instancetype)initWithObjects:(id)firstObj, ...
  public
  convenience init(objects elements: AnyObject...) {
    self.init(array: elements)
  }
}

extension NSSet {
  // - (instancetype)initWithObjects:(id)firstObj, ...
  public
  convenience init(objects elements: AnyObject...) {
    self.init(array: elements)
  }
}

extension NSSet : ArrayLiteralConvertible {
  public required convenience init(arrayLiteral elements: AnyObject...) {
    self.init(array: elements)
  }
}

extension NSOrderedSet : ArrayLiteralConvertible {
  public required convenience init(arrayLiteral elements: AnyObject...) {
    self.init(array: elements)
  }
}
