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


//===----------------------------------------------------------------------===//
// Enums
//===----------------------------------------------------------------------===//

// FIXME: one day this will be bridged from CoreFoundation and we
// should drop it here. <rdar://problem/14497260> (need support
// for CF bridging)
let kCFStringEncodingASCII: CFStringEncoding = 0x0600

// FIXME: <rdar://problem/16074941> NSStringEncoding doesn't work on 32-bit
typealias NSStringEncoding = UInt
let NSASCIIStringEncoding: UInt = 1
let NSNEXTSTEPStringEncoding: UInt = 2
let NSJapaneseEUCStringEncoding: UInt = 3
let NSUTF8StringEncoding: UInt = 4
let NSISOLatin1StringEncoding: UInt = 5
let NSSymbolStringEncoding: UInt = 6
let NSNonLossyASCIIStringEncoding: UInt = 7
let NSShiftJISStringEncoding: UInt = 8
let NSISOLatin2StringEncoding: UInt = 9
let NSUnicodeStringEncoding: UInt = 10
let NSWindowsCP1251StringEncoding: UInt = 11
let NSWindowsCP1252StringEncoding: UInt = 12
let NSWindowsCP1253StringEncoding: UInt = 13
let NSWindowsCP1254StringEncoding: UInt = 14
let NSWindowsCP1250StringEncoding: UInt = 15
let NSISO2022JPStringEncoding: UInt = 21
let NSMacOSRomanStringEncoding: UInt = 30
let NSUTF16StringEncoding: UInt = NSUnicodeStringEncoding
let NSUTF16BigEndianStringEncoding: UInt = 0x90000100
let NSUTF16LittleEndianStringEncoding: UInt = 0x94000100
let NSUTF32StringEncoding: UInt = 0x8c000100
let NSUTF32BigEndianStringEncoding: UInt = 0x98000100
let NSUTF32LittleEndianStringEncoding: UInt = 0x9c000100


//===----------------------------------------------------------------------===//
// NSObject
//===----------------------------------------------------------------------===//

// NSObject implements Equatable's == as -[NSObject isEqual:]
// NSObject implements Hashable's hashValue() as -[NSObject hash]
// FIXME: what about NSObjectProtocol?

extension NSObject : Equatable, Hashable {
  var hashValue: Int {
    return hash
  }
}

func == (lhs: NSObject, rhs: NSObject) -> Bool {
  return lhs.isEqual(rhs)
}

// This is a workaround for:
// <rdar://problem/16883288> Property of type 'String!' does not satisfy
// protocol requirement of type 'String'
extension NSObject : _PrintableNSObject {}

//===----------------------------------------------------------------------===//
// Strings
//===----------------------------------------------------------------------===//

@asmname("swift_convertStringToNSString")
func _convertStringToNSString(string: String) -> NSString {
  return string as NSString
}

func _convertNSStringToString(nsstring: NSString) -> String {
  return String(nsstring)
}

extension NSString : StringLiteralConvertible {
  class func convertFromExtendedGraphemeClusterLiteral(
    value: StaticString) -> Self {
    return convertFromStringLiteral(value)
  }

  class func convertFromStringLiteral(value: StaticString) -> Self {
    
    let immutableResult = NSString(
      bytesNoCopy: UnsafePointer<Void>(value.start),
      length: Int(value.byteSize),
      encoding:
        Bool(value.isASCII) ? NSASCIIStringEncoding : NSUTF8StringEncoding,
      freeWhenDone: false)
    
    return self(string: immutableResult)
  }
}


extension NSString {
  @conversion func __conversion() -> String {
    return String(self)
  }
}

//===----------------------------------------------------------------------===//
// New Strings
//===----------------------------------------------------------------------===//
extension NSString : _CocoaString {}

/// \brief Sets variables in Swift's core stdlib that allow it to
/// bridge Cocoa strings properly.  Currently invoked by a HACK in
/// Misc.mm; a better mechanism may be needed.
@asmname("__swift_initializeCocoaStringBridge")
func __swift_initializeCocoaStringBridge() -> COpaquePointer {
  _cocoaStringToContiguous = _cocoaStringToContiguousImpl
  _cocoaStringReadAll = _cocoaStringReadAllImpl
  _cocoaStringLength = _cocoaStringLengthImpl
  _cocoaStringSlice = _cocoaStringSliceImpl
  _cocoaStringSubscript = _cocoaStringSubscriptImpl
  return COpaquePointer()
}

// When used as a _CocoaString, an NSString should be either
// immutable or uniquely-referenced, and not have a buffer of
// contiguous UTF16.  Ideally these distinctions would be captured in
// the type system, so one would have to explicitly convert NSString
// to a type that conforms.  Unfortunately, we don't have a way to do
// that without an allocation (to wrap NSString in another class
// instance) or growing these protocol instances from one word to four
// (by way of allowing structs to conform).  Fortunately, correctness
// doesn't depend on these preconditions but efficiency might,
// because producing a _StringBuffer from an
// NSString-as-_CocoaString is assumed to require allocation and
// buffer copying.
//
func _cocoaStringReadAllImpl(
  source: _CocoaString, destination: UnsafePointer<UTF16.CodeUnit>) {
  let cfSelf: CFString = reinterpretCast(source)
  CFStringGetCharacters(
  cfSelf, CFRange(location: 0, length: CFStringGetLength(cfSelf)), destination)
}
  
func _cocoaStringToContiguousImpl(
  source: _CocoaString, range: Range<Int>, minimumCapacity: Int
) -> _StringBuffer {
  let cfSelf: CFString = reinterpretCast(source)
  assert(CFStringGetCharactersPtr(cfSelf).isNull(),
    "Known contiguously-stored strings should already be converted to Swift")

  var startIndex = range.startIndex
  var count = range.endIndex - startIndex

  var buffer = _StringBuffer(capacity: max(count, minimumCapacity), 
                             initialSize: count, elementWidth: 2)

  CFStringGetCharacters(
    cfSelf, CFRange(location: startIndex, length: count), 
    UnsafePointer<UniChar>(buffer.start))
  
  return buffer
}

func _cocoaStringLengthImpl(source: _CocoaString) -> Int {
  // FIXME: Not ultra-fast, but reliable... but we're counting on an
  // early demise for this function anyhow
  return (source as NSString)!.length
}

func _cocoaStringSliceImpl(
  target: _StringCore, subRange: Range<Int>) -> _StringCore {
  
  let buffer = NSOpaqueString(
    owner: String(target), 
    subRange:
      NSRange(location: subRange.startIndex, length: countElements(subRange)))
  
  return _StringCore(
    baseAddress: nil,
    count: countElements(subRange),
    elementShift: 1,
    hasCocoaBuffer: true,
    owner: buffer)
}

func _cocoaStringSubscriptImpl(
  target: _StringCore, position: Int) -> UTF16.CodeUnit {
  // FIXME: implement this in terms of CFString
  return (target.cocoaBuffer! as NSString)!.characterAtIndex(position)
}


//
// NSString slice subclasses created when converting String to
// NSString.  We take care to avoid creating long wrapper chains
// when converting back and forth.
//

/// An NSString built around a slice of contiguous Swift String storage
class NSContiguousString : NSString {
  init(_ value: _StringCore) {
    assert(
      value.hasContiguousStorage,
      "NSContiguousString requires contiguous storage")
    self.value = value
    super.init()
  }

  func length() -> Int {
    return value.count
  }

  override func characterAtIndex(index: Int) -> unichar {
    return value[index]
  }

  override func getCharacters(buffer: CMutablePointer<unichar>,
                              range aRange: NSRange) {
    assert(aRange.location + aRange.length <= Int(value.count))

    if value.elementWidth == 2 {
      UTF16.copy(
        value.startUTF16 + aRange.location, destination: UnsafePointer(buffer),
        count: aRange.length)
    }
    else {
      UTF16.copy(
        value.startASCII + aRange.location, destination: UnsafePointer(buffer),
        count: aRange.length)
    }
  }

  @objc
  func _fastCharacterContents() -> UnsafePointer<unichar> {
    return value.elementWidth == 2 ? UnsafePointer(value.startUTF16) : nil
  }

  //
  // Implement sub-slicing without adding layers of wrapping
  // 
  override func substringFromIndex(start: Int) -> String {
    return NSContiguousString(value[Int(start)..Int(value.count)])
  }

  override func substringToIndex(end: Int) -> String {
    return NSContiguousString(value[0..Int(end)])
  }

  override func substringWithRange(aRange: NSRange) -> String {
    return NSContiguousString(
      value[Int(aRange.location)..Int(aRange.location + aRange.length)])
  }

  //
  // Implement copy; since this string is immutable we can just return ourselves
  override func copy() -> AnyObject {
    return self
  }

  let value: _StringCore
}

// A substring of an arbitrary immutable NSString.  The underlying
// NSString is assumed to not provide contiguous UTF16 storage.
class NSOpaqueString : NSString {
  func length() -> Int {
    return subRange.length
  }

  override func characterAtIndex(index: Int) -> unichar {
    return owner.characterAtIndex(index + subRange.location)
  }

  override func getCharacters(buffer: CMutablePointer<unichar>,
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
    return NSOpaqueString(
             owner: owner, 
             subRange: NSRange(location: subRange.location + start, 
                               length: subRange.length - start))
  }

  override func substringToIndex(end: Int) -> String {
    return NSOpaqueString(
             owner: owner, 
             subRange: NSRange(location: subRange.location, length: end))
  }

  override func substringWithRange(aRange: NSRange) -> String {
    return NSOpaqueString(
             owner: owner, 
             subRange: NSRange(location: aRange.location + subRange.location, 
                               length: aRange.length))
  }

  init(owner: String, subRange: NSRange) {
    self.owner = owner
    self.subRange = subRange
    super.init()
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
// Conversion from Swift's native representations to NSString
// 
extension String {
  @conversion func __conversion() -> NSString {
    if let ns = core.cocoaBuffer {
      if _cocoaStringLength(source: ns) == core.count {
        return (ns as NSString)!
      }
    }
    assert(core.hasContiguousStorage)
    return NSContiguousString(core)
  }
}

//
// Conversion from NSString to Swift's native representation
//
extension String {
  init(_ value: NSString) {
    if let wrapped = value as NSContiguousString {
      self.core = wrapped.value
      return
    }
    
    // Treat it as a CF object because presumably that's what these
    // things tend to be, and CF has a fast path that avoids
    // objc_msgSend
    let cfValue: CFString = reinterpretCast(value)

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
    var start = UnsafePointer<RawByte>(nulTerminatedASCII._bytesPtr)
    let isUTF16 = nulTerminatedASCII.isNull()
    if (isUTF16) {
      start = UnsafePointer(CFStringGetCharactersPtr(cfImmutableValue))
    }

    self.core = _StringCore(
      baseAddress: reinterpretCast(start),
      count: length,
      elementShift: isUTF16 ? 1 : 0,
      hasCocoaBuffer: true,
      owner: reinterpretCast(cfImmutableValue))
  }
}

extension String : _BridgedToObjectiveC {
  static func getObjectiveCType() -> Any.Type {
    return NSString.self
  }

  func bridgeToObjectiveC() -> NSString {
    return self
  }

  static func bridgeFromObjectiveC(x: NSString) -> String {
    return String(x)
  }
}

//===----------------------------------------------------------------------===//
// Numbers
//===----------------------------------------------------------------------===//

// Conversions between NSNumber and various numeric types. The
// conversion to NSNumber is automatic (auto-boxing), while conversion
// back to a specific numeric type requires a cast.
// FIXME: Incomplete list of types.
extension Int : _BridgedToObjectiveC {
  init(_ number: NSNumber) {
    value = number.integerValue.value
  }

  @conversion func __conversion() -> NSNumber {
    return NSNumber(integer: self)
  }

  static func getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }

  func bridgeToObjectiveC() -> NSNumber {
    return self
  }

  static func bridgeFromObjectiveC(x: NSNumber) -> Int {
    fatal("implement")
  }
}

extension UInt : _BridgedToObjectiveC {
  init(_ number: NSNumber) {
    value = number.unsignedIntegerValue.value
  }

  @conversion func __conversion() -> NSNumber {
    // FIXME: Need a blacklist for certain methods that should not
    // import NSUInteger as Int.
    return NSNumber(unsignedInteger: Int(value))
  }

  static func getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }

  func bridgeToObjectiveC() -> NSNumber {
    return self
  }

  static func bridgeFromObjectiveC(x: NSNumber) -> UInt {
    fatal("implement")
  }
}

extension Float : _BridgedToObjectiveC {
  init(_ number: NSNumber) {
    value = number.floatValue.value
  }

  @conversion func __conversion() -> NSNumber {
    return NSNumber(float: self)
  }

  static func getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }

  func bridgeToObjectiveC() -> NSNumber {
    return self
  }

  static func bridgeFromObjectiveC(x: NSNumber) -> Float {
    fatal("implement")
  }
}

extension Double : _BridgedToObjectiveC {
  init(_ number: NSNumber) {
    value = number.doubleValue.value
  }

  @conversion func __conversion() -> NSNumber {
    return NSNumber(double: self)
  }

  static func getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }

  func bridgeToObjectiveC() -> NSNumber {
    return self
  }

  static func bridgeFromObjectiveC(x: NSNumber) -> Double {
    fatal("implement")
  }
}

extension Bool: _BridgedToObjectiveC {
  init(_ number: NSNumber) {
    if number.boolValue { self = Bool.true }
    else { self = Bool.false }
  }

  @conversion func __conversion() -> NSNumber {
    return NSNumber(bool: self)
  }

  static func getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }

  func bridgeToObjectiveC() -> NSNumber {
    return self
  }

  static func bridgeFromObjectiveC(x: NSNumber) -> Bool {
    fatal("implement")
  }
}

// Literal support for NSNumber
extension NSNumber : FloatLiteralConvertible, IntegerLiteralConvertible {
  class func convertFromIntegerLiteral(value: Int) -> NSNumber {
    return NSNumber(integer: value)
  }

  class func convertFromFloatLiteral(value: Double) -> NSNumber {
    return NSNumber(double: value)
  }
}

let NSNotFound: Int = .max

//===----------------------------------------------------------------------===//
// Arrays
//===----------------------------------------------------------------------===//

extension NSArray : ArrayLiteralConvertible {
  class func convertFromArrayLiteral(elements: AnyObject...) -> Self {
    // + (instancetype)arrayWithObjects:(const id [])objects count:(NSUInteger)cnt;
    let x = _extractOrCopyToNativeArrayBuffer(elements.buffer)
    let result = self(objects: UnsafePointer(x.elementStorage), count: x.count)
    _fixLifetime(x)
    return result
  }
}

// Should be in an extension, pending <rdar://problem/16536656>
func asNSArray<T>(array: T[]) -> NSArray {
  return (array.asCocoaArray() as AnyObject)!
}

/// The entry point for bridging `NSArray` to `Array`.
func _convertNSArrayToArray<T>(nsarr: NSArray) -> T[] {
  return T[](ArrayBuffer(reinterpretCast(nsarr) as CocoaArray))
}

/// The entry point for bridging 'Array' to 'NSArray'.
func _convertArrayToNSArray<T>(arr: T[]) -> NSArray {
  // FIXME: Check conditional bridging here?
  return arr.bridgeToObjectiveC()
}

extension Array : _ConditionallyBridgedToObjectiveC {
  static func isBridgedToObjectiveC() -> Bool {
    return Swift.isBridgedToObjectiveC(T.self)
  }

  static func getObjectiveCType() -> Any.Type {
    return NSArray.self
  }

  func bridgeToObjectiveC() -> NSArray {
    return reinterpretCast(self.buffer.asCocoaArray())
  }

  static func bridgeFromObjectiveC(x: NSArray) -> Array<T> {
    fatal("implement")
  }

  @conversion func __conversion() -> NSArray {
    return self.bridgeToObjectiveC()
  }
}

extension NSArray : Reflectable {
  func getMirror() -> Mirror {
    return reflect(self as AnyObject[])
  }
}

//===----------------------------------------------------------------------===//
// Dictionaries
//===----------------------------------------------------------------------===//

extension NSDictionary : DictionaryLiteralConvertible {
  class func convertFromDictionaryLiteral(elements: (NSCopying, AnyObject)...)
               -> Self {
    var keys = new (NSCopying?)[elements.count] {elements[$0].0}
    var objects = new (AnyObject?)[elements.count] {elements[$0].1}

    // FIXME: init closures currently ignored <rdar://problem/15024561>
    for i in 0..elements.count {
      keys[i] = elements[i].0
      objects[i] = elements[i].1
    }

    return self(objects: objects, forKeys: keys, count: elements.count)
  }
}

/// The entry point for bridging `NSDictionary` to `Dictionary`.
func _convertNSDictionaryToDictionary<K: NSObject, V: AnyObject>(
       d: NSDictionary
     ) -> Dictionary<K, V> {
  return Dictionary<K, V>(_cocoaDictionary: reinterpretCast(d))
}

/// The entry point for bridging `Dictionary` to `NSDictionary`.
func _convertDictionaryToNSDictionary<KeyType, ValueType>(
    d: Dictionary<KeyType, ValueType>
) -> NSDictionary {
  switch d._variantStorage {
  case .Native(let nativeOwner):
    securityCheck(isBridgedToObjectiveC(KeyType.self),
                  "KeyType is not bridged to Objective-C")
    securityCheck(isBridgedToObjectiveC(ValueType.self),
                  "ValueType is not bridged to Objective-C")

    let isKeyBridgedVerbatim = isBridgedVerbatimToObjectiveC(KeyType.self)
    let isValueBridgedVerbatim = isBridgedVerbatimToObjectiveC(ValueType.self)

    // If both `KeyType` and `ValueType` can be bridged verbatim, return the
    // underlying storage.
    if _fastPath(isKeyBridgedVerbatim && isValueBridgedVerbatim) {
      let anNSSwiftDictionary: NSSwiftDictionary = nativeOwner
      return reinterpretCast(anNSSwiftDictionary)
    }

    // At least either one of `KeyType` or `ValueType` can not be bridged
    // verbatim.  Bridge all the contents eagerly and create an `NSDictionary`.
    let nativeStorage = nativeOwner.nativeStorage
    let result = NSMutableDictionary(capacity: nativeStorage.count)
    let endIndex = nativeStorage.endIndex
    for var i = nativeStorage.startIndex; i != endIndex; ++i {
      let (key, value) = nativeStorage.assertingGet(i)
      var bridgedKey: AnyObject
      if _fastPath(isKeyBridgedVerbatim) {
        // Avoid calling the runtime.
        bridgedKey = _reinterpretCastToAnyObject(key)
      } else {
        bridgedKey = bridgeToObjectiveC(key)!
      }
      var bridgedValue: AnyObject
      if _fastPath(isValueBridgedVerbatim) {
        // Avoid calling the runtime.
        bridgedValue = _reinterpretCastToAnyObject(value)
      } else {
        bridgedValue = bridgeToObjectiveC(value)!
      }

      // NOTE: the just-bridged key is copied here.  It would be nice to avoid
      // copying it, but it seems like there is no public APIs for this.  But
      // since the key may potentially come from user code, it might be a good
      // idea to copy it anyway.  In the case of bridging stdlib types, this is
      // wasteful.
      if let nsCopyingKey = bridgedKey as NSCopying {
        result[nsCopyingKey] = bridgedValue
      } else {
        fatal("key bridged to an object that does not conform to NSCopying")
      }
    }
    return reinterpretCast(result)

  case .Cocoa(let cocoaStorage):
    // The `Dictionary` is already backed by `NSDictionary` of some kind.  Just
    // unwrap it.
    return reinterpretCast(cocoaStorage.cocoaDictionary)
  }
}

// Dictionary<KeyType, ValueType> is conditionally bridged to NSDictionary
extension Dictionary : _ConditionallyBridgedToObjectiveC {
  static func isBridgedToObjectiveC() -> Bool {
    return KeyType.self is NSObject.Type &&
           Swift.isBridgedVerbatimToObjectiveC(ValueType.self)
  }

  static func getObjectiveCType() -> Any.Type {
    return NSDictionary.self
  }

  func bridgeToObjectiveC() -> NSDictionary {
    return _convertDictionaryToNSDictionary(self)
  }

  static func bridgeFromObjectiveC(x: NSDictionary) -> Dictionary {
    fatal("implement")
  }
}

extension NSDictionary {
  @conversion
  func __conversion() -> Dictionary<NSObject, AnyObject> {
    return _convertNSDictionaryToDictionary(reinterpretCast(self))
  }
}

extension Dictionary {
  @conversion
  func __conversion() -> NSDictionary {
    return _convertDictionaryToNSDictionary(self)
  }
}

extension NSDictionary : Reflectable {
  func getMirror() -> Mirror {
    let dict : Dictionary<NSObject,AnyObject> = _convertNSDictionaryToDictionary(self)
    return reflect(dict)
  }
}

//===----------------------------------------------------------------------===//
// General objects
//===----------------------------------------------------------------------===//

extension NSObject : CVarArg {
  func encode() -> Word[] {
    _autorelease(self)
    return encodeBitsAsWords(self)
  }
}

//===----------------------------------------------------------------------===//
// Fast enumeration
//===----------------------------------------------------------------------===//

// Give NSFastEnumerationState a default initializer, for convenience.
extension NSFastEnumerationState {
  init() {
    state = 0
    itemsPtr = .null()
    mutationsPtr = .null()
    extra = (0,0,0,0,0)
  }
}


// NB: This is a class because fast enumeration passes around interior pointers
// to the enumeration state, so the state cannot be moved in memory. We will
// probably need to implement fast enumeration in the compiler as a primitive
// to implement it both correctly and efficiently.
class NSFastGenerator : Generator {
  var enumerable: NSFastEnumeration
  var state: NSFastEnumerationState[]
  var n: Int
  var count: Int

  /// Size of ObjectsBuffer, in ids.
  var STACK_BUF_SIZE: Int { return 4 }

  /// Must have enough space for STACK_BUF_SIZE object references.
  struct ObjectsBuffer {
    var buf = (COpaquePointer(), COpaquePointer(),
               COpaquePointer(), COpaquePointer())
  }
  var objects: ObjectsBuffer[]

  func next() -> AnyObject? {
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
    count = enumerable.countByEnumeratingWithState(state.elementStorage,
      objects: UnsafePointer<AnyObject?>(objects.elementStorage),
      count: STACK_BUF_SIZE)
  }

  init(_ enumerable: NSFastEnumeration) {
    self.enumerable = enumerable
    self.state = new NSFastEnumerationState[1]
    self.state[0].state = 0
    self.objects = new ObjectsBuffer[1]
    self.n = -1
    self.count = -1
  }
}

extension NSArray : Sequence {
  func generate() -> NSFastGenerator {
    return NSFastGenerator(self)
  }
}

// FIXME: This should not be necessary.  We
// should get this from the extension on 'NSArray' above.
extension NSMutableArray : Sequence {}

extension NSSet : Sequence {
  func generate() -> NSFastGenerator {
    return NSFastGenerator(self)
  }
}

// FIXME: This should not be necessary.  We
// should get this from the extension on 'NSSet' above.
extension NSMutableSet : Sequence {}

// FIXME: A class because we can't pass a struct with class fields through an
// [objc] interface without prematurely destroying the references.
class NSDictionaryGenerator : Generator {
  var fastGenerator : NSFastGenerator
  var dictionary : NSDictionary {
    return (fastGenerator.enumerable as NSDictionary)!
  }

  func next() -> (key: AnyObject, value: AnyObject)? {
    switch fastGenerator.next() {
    case .None:
      return .None
    case .Some(var key):
      // Deliberately avoid the subscript operator in case the dictionary
      // contains non-copyable keys. This is rare since NSMutableDictionary
      // requires them, but we don't want to paint ourselves into a corner.
      return (key: key, value: dictionary.objectForKey(key))
    }
  }

  init(_ dict: NSDictionary) {
    self.fastGenerator = NSFastGenerator(dict)
  }
}

extension NSDictionary : Sequence {
  func generate() -> NSDictionaryGenerator {
    return NSDictionaryGenerator(self)
  }
}

// FIXME: This should not be necessary.  We
// should get this from the extension on 'NSDictionary' above.
extension NSMutableDictionary : Sequence {}

//===----------------------------------------------------------------------===//
// Ranges
//===----------------------------------------------------------------------===//

extension NSRange {
  init(_ x: Range<Int>) {
    location = x.startIndex
    length = countElements(x)
  }
  @conversion func __conversion() -> Range<Int> {
    return Range(start: location, end: location + length)
  }
}

//===----------------------------------------------------------------------===//
// NSZone
//===----------------------------------------------------------------------===//

struct NSZone {
  var pointer : COpaquePointer

  init() { pointer = nil }
}

extension _Nil {
  @conversion func __conversion() -> NSZone {
    return NSZone()
  }
}


//===----------------------------------------------------------------------===//
// NSLocalizedString
//===----------------------------------------------------------------------===//

/// Returns a localized string, using the main bundle if one is not specified.
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
func _getObjCChild(Int, _MagicMirrorData) -> (String, Mirror)

func _getObjCSummary(data: _MagicMirrorData) -> String {
  // FIXME: Trying to call debugDescription on AnyObject crashes.
  // <rdar://problem/16349526>
  // Work around by reinterpretCasting to NSObject and hoping for the best.
  return (data._loadValue() as NSObject).debugDescription
}

@asmname("swift_ObjCMirror_quickLookObject")
func _getObjCQuickLookObject(data: _MagicMirrorData) -> QuickLookObject?

struct _ObjCMirror: Mirror {
  let data: _MagicMirrorData

  var value: Any { return data.objcValue }
  var valueType: Any.Type { return data.objcValueType }
  var objectIdentifier: ObjectIdentifier? {
    return data._loadValue() as ObjectIdentifier
  }
  var count: Int {
    return _getObjCCount(data)
  }
  subscript(i: Int) -> (String, Mirror) {
    return _getObjCChild(i, data)
  }
  var summary: String {
    return _getObjCSummary(data)
  }
  var quickLookObject: QuickLookObject? {
    return _getObjCQuickLookObject(data)
  }
  var disposition: MirrorDisposition { return .Class }
}

struct _ObjCSuperMirror: Mirror {
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
  subscript(i: Int) -> (String, Mirror) {
    return _getObjCChild(i, data)
  }
  var summary: String {
    return _getObjCSummary(data)
  }
  var quickLookObject: QuickLookObject? {
    return _getObjCQuickLookObject(data)
  }
  var disposition: MirrorDisposition { return .Class }
}

struct _NSURLMirror : Mirror {
  var _u : NSURL
  
  init(_ u : NSURL) {_u = u}
  
  var value : Any { get { return _u } }
  
  var valueType : Any.Type { get { return (_u as Any).dynamicType } }
  
  var objectIdentifier: ObjectIdentifier? { get { return .None } }
  
  var count: Int { get { return 0 } }
  
  subscript(_: Int) -> (String,Mirror) { get { fatal("don't ask") } }
  
  var summary: String { get { return _u.absoluteString } }
  
  var quickLookObject: QuickLookObject? { get { return .Some(.URL(summary)) } }
  
  var disposition : MirrorDisposition { get { return .Aggregate } }
}

extension NSURL : Reflectable {
  func getMirror() -> Mirror {
    return _NSURLMirror(self)
  }
}

struct _NSRangeMirror : Mirror {
  var _r : NSRange
  
  init(_ r : NSRange) {_r = r}
  
  var value : Any { get { return _r } }
  
  var valueType : Any.Type { get { return (_r as Any).dynamicType } }
  
  var objectIdentifier: ObjectIdentifier? { get { return .None } }
  
  var count: Int { get { return 2 } }
  
  subscript(i: Int) -> (String,Mirror) {
    switch i {
      case 0: return ("location",reflect(_r.location))
      case 1: return ("length",reflect(_r.length))
      default: fatal("don't ask")
    }
  }
  
  var summary: String { return "(\(_r.location),\(_r.length))" }
  
  var quickLookObject: QuickLookObject? { return .Some(.Range(UInt64(_r.location),UInt64(_r.length))) }
  
  var disposition : MirrorDisposition { return .Aggregate }
}

extension NSRange : Reflectable {
  func getMirror() -> Mirror {
    return _NSRangeMirror(self)
  }
}

extension NSString : Reflectable {
  func getMirror() -> Mirror {
    return reflect(self as String)
  }
}

//===----------------------------------------------------------------------===//
// NSDate
//===----------------------------------------------------------------------===//

struct _NSDateMirror : Mirror {
  var _d : NSDate
  
  init(_ d : NSDate) {_d = d}
  
  var value : Any { get { return _d } }
  
  var valueType : Any.Type { get { return (_d as Any).dynamicType } }
  
  var objectIdentifier: ObjectIdentifier? { get { return .None } }
  
  var count: Int { get { return 0 } }
  
  subscript(i: Int) -> (String,Mirror) {
    fatal("don't ask")
  }
  
  var summary: String {
    let df = NSDateFormatter()
    df.dateStyle = .MediumStyle
    df.timeStyle = .ShortStyle
    return df.stringFromDate(_d)
  }
  
  var quickLookObject: QuickLookObject? { return .Some(.Text(summary)) }
  
  var disposition : MirrorDisposition { return .Aggregate }
}

extension NSDate : Reflectable {
  func getMirror() -> Mirror {
    return _NSDateMirror(self)
  }
}

//===----------------------------------------------------------------------===//
// NSLog
//===----------------------------------------------------------------------===//

func NSLog(format: String, args: CVarArg...) {
  withVaList(args) { NSLogv(format, $0) }
}

//===----------------------------------------------------------------------===//
// Variadic initializers and methods
//===----------------------------------------------------------------------===//

extension NSPredicate {
  // + (NSPredicate *)predicateWithFormat:(NSString *)predicateFormat, ...;
  convenience init(format predicateFormat: String, _ args: CVarArg...) {
    let va_args = getVaList(args)
    return self.init(format: predicateFormat, arguments: va_args)
  }
}

extension NSExpression {
  // + (NSExpression *) expressionWithFormat:(NSString *)expressionFormat, ...;
  convenience init(format expressionFormat: String, _ args: CVarArg...) {
    let va_args = getVaList(args)
    return self.init(format: expressionFormat, arguments: va_args)
  }
}

extension NSString {
  convenience init(format: NSString, _ args: CVarArg...) {
    // We can't use withVaList because 'self' cannot be captured by a closure
    // before it has been initialized.
    let va_args = getVaList(args)
    self.init(format: format, arguments: va_args)
  }
  convenience init(format: NSString, locale: NSLocale?, _ args: CVarArg...) {
    // We can't use withVaList because 'self' cannot be captured by a closure
    // before it has been initialized.
    let va_args = getVaList(args)
    self.init(format: format, locale: locale, arguments: va_args)
  }

  class func localizedStringWithFormat(format: NSString,
                                       _ args: CVarArg...) -> NSString {
    return withVaList(args) {
      NSString(format: format, locale: NSLocale.currentLocale(),
               arguments: $0)
    }
  }

  func stringByAppendingFormat(format: NSString, _ args: CVarArg...)
  -> NSString {
    return withVaList(args) {
      self.stringByAppendingString(NSString(format: format, arguments: $0))
    }
  }
}

extension NSMutableString {
  func appendFormat(format: NSString, _ args: CVarArg...) {
    return withVaList(args) {
      self.appendString(NSString(format: format, arguments: $0))
    }
  }
}

// FIXME. Writing test to use the initializer is blocked by rdar://16801456
extension NSArray {
  // Overlay: - (instancetype)initWithObjects:(id)firstObj, ...
  convenience init(objects elements: AnyObject...) {
    // - (instancetype)initWithObjects:(const id [])objects count:(NSUInteger)cnt;
    let x = _extractOrCopyToNativeArrayBuffer(elements.buffer)
    // Use Imported:
    // @objc(initWithObjects:count:)
    //    init(withObjects objects: CConstPointer<AnyObject?>,
    //    count cnt: Int)
    self.init(objects: UnsafePointer(x.elementStorage), count: x.count)
    _fixLifetime(x)
  }
  
  var _asCocoaArray: CocoaArray {
    return reinterpretCast(self) as CocoaArray
  }
  
  @conversion
  func __conversion() -> Array<AnyObject> {
    return Array(ArrayBuffer(self._asCocoaArray.copyWithZone(nil)))
  }
}

extension NSDictionary {
  // - (instancetype)initWithObjectsAndKeys:(id)firstObject, ...
  convenience init(objectsAndKeys objects: AnyObject...) {
    // - (instancetype)initWithObjects:(NSArray *)objects forKeys:(NSArray *)keys;
    var values: AnyObject[] = []
    var keys:   AnyObject[] = []
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
  convenience init(objects elements: AnyObject...) {
    let x = _extractOrCopyToNativeArrayBuffer(elements.buffer)
    // - (instancetype)initWithObjects:(const id [])objects count:(NSUInteger)cnt;
    // Imported as:
    // @objc(initWithObjects:count:)
    // init(withObjects objects: CConstPointer<AnyObject?>,
    //      count cnt: Int)
    self.init(objects: UnsafePointer(x.elementStorage), count: x.count)
    _fixLifetime(x)
  }
}

extension NSSet {
  // - (instancetype)initWithObjects:(id)firstObj, ...
  convenience init(objects elements: AnyObject...) {
    let x = _extractOrCopyToNativeArrayBuffer(elements.buffer)
    // - (instancetype)initWithObjects:(const id [])objects count:(NSUInteger)cnt;
    // Imported as:
    // @objc(initWithObjects:count:)
    // init(withObjects objects: CConstPointer<AnyObject?>, count cnt: Int)
    self.init(objects: UnsafePointer(x.elementStorage), count: x.count)
    _fixLifetime(x)
  }
}

struct _NSSetMirror : Mirror {
  var _s : NSSet
  var _a : NSArray!
  
  init(_ s : NSSet) {
    _s = s
    _a = _s.allObjects
  }
  
  var value : Any { get { return _s } }
  
  var valueType : Any.Type { get { return (_s as Any).dynamicType } }
  
  var objectIdentifier: ObjectIdentifier? { get { return .None } }
  
  var count: Int { 
    if _a {
      return _a.count
    }
    return 0
  }
  
  subscript(i: Int) -> (String,Mirror) {
    if i >= 0 && i < count {
      return ("[\(i)]",reflect(_a[i]))
    }
    fatal("don't ask")
  }
  
  var summary: String { return "\(count) elements" }
  
  var quickLookObject: QuickLookObject? { return nil }
  
  var disposition : MirrorDisposition { return .MembershipContainer }
}

extension NSSet : Reflectable {
  func getMirror() -> Mirror {
    return _NSSetMirror(self)
  }
}

