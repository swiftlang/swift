@_exported import ObjectiveC
@_exported import CoreGraphics
@_exported import Foundation

@_silgen_name("swift_StringToNSString") internal
func _convertStringToNSString(_ string: String) -> NSString

@_silgen_name("swift_NSStringToString") internal
func _convertNSStringToString(_ nsstring: NSString?) -> String

public func == (lhs: NSObject, rhs: NSObject) -> Bool {
  return lhs.isEqual(rhs)
}

public let NSUTF8StringEncoding: UInt = 8

// NSArray bridging entry points
func _convertNSArrayToArray<T>(_ nsarr: NSArray?) -> [T] {
  return [T]()
}

func _convertArrayToNSArray<T>(_ arr: [T]) -> NSArray {
  return NSArray()
}

// NSDictionary bridging entry points
internal func _convertDictionaryToNSDictionary<Key, Value>(
    _ d: Dictionary<Key, Value>
) -> NSDictionary {
  return NSDictionary()
}

internal func _convertNSDictionaryToDictionary<K: NSObject, V: AnyObject>(
       _ d: NSDictionary?
     ) -> Dictionary<K, V> {
  return Dictionary<K, V>()
}

// NSSet bridging entry points
internal func _convertSetToNSSet<T : Hashable>(_ s: Set<T>) -> NSSet {
  return NSSet()
}

internal func _convertNSSetToSet<T : Hashable>(_ s: NSSet?) -> Set<T> {
  return Set<T>()
}

extension AnyHashable : _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> NSObject {
    return NSObject()
  }
  public static func _forceBridgeFromObjectiveC(_ x: NSObject,
                                                result: inout AnyHashable?) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSObject,
    result: inout AnyHashable?
  ) -> Bool {
    return true
  }
  public static func _unconditionallyBridgeFromObjectiveC(_ x: NSObject?) -> AnyHashable {
    return AnyHashable("")
 }
}

extension String : _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> NSString {
    return NSString()
  }
  public static func _forceBridgeFromObjectiveC(_ x: NSString,
                                                result: inout String?) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSString,
    result: inout String?
  ) -> Bool {
    return true
  }
  public static func _unconditionallyBridgeFromObjectiveC(_ x: NSString?) -> String {
    return String()
 }
}

extension Int : _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> NSNumber {
    return NSNumber()
  }
  public static func _forceBridgeFromObjectiveC(
    _ x: NSNumber, 
    result: inout Int?
  ) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSNumber,
    result: inout Int?
  ) -> Bool {
    return true
  }
  public static func _unconditionallyBridgeFromObjectiveC(
    _ x: NSNumber?
  ) -> Int {
    return 0
  }
}

extension Bool: _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> NSNumber {
    return NSNumber()
  }
  public static func _forceBridgeFromObjectiveC(
    _ x: NSNumber, 
    result: inout Bool?
  ) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSNumber,
    result: inout Bool?
  ) -> Bool {
    return true
  }
  public static func _unconditionallyBridgeFromObjectiveC(
    _ x: NSNumber?
  ) -> Bool {
    return false
  }
}

extension Array : _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> NSArray {
    return NSArray()
  }
  public static func _forceBridgeFromObjectiveC(
    _ x: NSArray,
    result: inout Array?
  ) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSArray,
    result: inout Array?
  ) -> Bool {
    return true
  }
  public static func _unconditionallyBridgeFromObjectiveC(
    _ x: NSArray?
  ) -> Array {
    return Array()
  }
}

extension Dictionary : _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> NSDictionary {
    return NSDictionary()
  }
  public static func _forceBridgeFromObjectiveC(
    _ x: NSDictionary,
    result: inout Dictionary?
  ) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSDictionary,
    result: inout Dictionary?
  ) -> Bool {
    return true
  }
  public static func _unconditionallyBridgeFromObjectiveC(
    _ x: NSDictionary?
  ) -> Dictionary {
    return Dictionary()
  }
}

extension Set : _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> NSSet {
    return NSSet()
  }
  public static func _forceBridgeFromObjectiveC(
    _ x: NSSet,
    result: inout Set?
  ) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSSet,
    result: inout Set?
  ) -> Bool {
    return true
  }
  public static func _unconditionallyBridgeFromObjectiveC(
    _ x: NSSet?
  ) -> Set {
    return Set()
  }
}

extension CGFloat : _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> NSNumber {
    return NSNumber()
  }
  public static func _forceBridgeFromObjectiveC(
    _ x: NSNumber,
    result: inout CGFloat?
  ) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSNumber,
    result: inout CGFloat?
  ) -> Bool {
    return true
  }
  public static func _unconditionallyBridgeFromObjectiveC(
    _ x: NSNumber?
  ) -> CGFloat {
    return CGFloat()
  }
}

extension NSRange : _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> NSValue {
    return NSValue()
  }

  public static func _forceBridgeFromObjectiveC(
    _ x: NSValue,
    result: inout NSRange?
  ) {
    result = x.rangeValue
  }
  
  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSValue,
    result: inout NSRange?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }

  public static func _unconditionallyBridgeFromObjectiveC(
    _ x: NSValue?
  ) -> NSRange {
    return NSRange()
  }
}

public struct URL : _ObjectiveCBridgeable {
  public init() { }

  public init?(string: String) { return nil }

  public func _bridgeToObjectiveC() -> NSURL {
    return NSURL()
  }
  public static func _forceBridgeFromObjectiveC(_ x: NSURL,
                                                result: inout URL?) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSURL,
    result: inout URL?
  ) -> Bool {
    return true
  }
  public static func _unconditionallyBridgeFromObjectiveC(_ x: NSURL?) -> URL {
    return URL()
 }
}

extension NSError : Error {
  public var _domain: String { return domain }
  public var _code: Int { return code }
}

extension NSArray {
  @objc(methodIntroducedInOverlay) public func introducedInOverlay() { }
}

@_silgen_name("swift_convertNSErrorToError")
func _convertNSErrorToError(_ string: NSError?) -> Error

@_silgen_name("swift_convertErrorToNSError")
func _convertErrorToNSError(_ string: Error) -> NSError

/// An internal protocol to represent Swift error enums that map to standard
/// Cocoa NSError domains.
public protocol _ObjectiveCBridgeableError : Error {
  /// Produce a value of the error type corresponding to the given NSError,
  /// or return nil if it cannot be bridged.
  init?(_bridgedNSError: NSError)
}

/// Describes a bridged error that stores the underlying NSError, so
/// it can be queried.
public protocol _BridgedStoredNSError : _ObjectiveCBridgeableError {
  /// The type of an error code.
  associatedtype Code: _ErrorCodeProtocol

  /// The error code for the given error.
  var code: Code { get }

  //// Retrieves the embedded NSError.
  var _nsError: NSError { get }

  /// Create a new instance of the error type with the given embedded
  /// NSError.
  ///
  /// The \c error must have the appropriate domain for this error
  /// type.
  init(_nsError error: NSError)
}

public protocol _ErrorCodeProtocol {
  /// The corresponding error code.
  associatedtype _ErrorType
}

public extension _BridgedStoredNSError {
  public init?(_bridgedNSError error: NSError) {
    self.init(_nsError: error)
  }
}

/// Various helper implementations for _BridgedStoredNSError
public extension _BridgedStoredNSError
    where Code: RawRepresentable, Code.RawValue: SignedInteger {
  // FIXME: Generalize to Integer.
  public var code: Code {
    return Code(rawValue: numericCast(_nsError.code))!
  }

  /// Initialize an error within this domain with the given ``code``
  /// and ``userInfo``.
  public init(_ code: Code, userInfo: [String : Any] = [:]) {
    self.init(_nsError: NSError(domain: "", code: 0, userInfo: [:]))
  }

  /// The user-info dictionary for an error that was bridged from
  /// NSError.
  var userInfo: [String : Any] { return [:] }
}

/// Various helper implementations for _BridgedStoredNSError
public extension _BridgedStoredNSError
    where Code: RawRepresentable, Code.RawValue: UnsignedInteger {
  // FIXME: Generalize to Integer.
  public var code: Code {
    return Code(rawValue: numericCast(_nsError.code))!
  }

  /// Initialize an error within this domain with the given ``code``
  /// and ``userInfo``.
  public init(_ code: Code, userInfo: [String : Any] = [:]) {
    self.init(_nsError: NSError(domain: "", code: 0, userInfo: [:]))
  }
}

extension NSDictionary {
  public subscript(_: Any) -> Any? {
    @objc(_swift_objectForKeyedSubscript:)
    get { fatalError() }
  }

  public func nonObjCExtensionMethod<T>(_: T) {}
}
extension NSMutableDictionary {
  public override subscript(_: Any) -> Any? {
    get { fatalError() }
    @objc(_swift_setObject:forKeyedSubscript:)
    set { }
  }
}
