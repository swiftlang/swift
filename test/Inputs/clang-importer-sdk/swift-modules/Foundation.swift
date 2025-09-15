@_exported import ObjectiveC
@_exported import CoreGraphics
@_exported import Foundation

public let NSUTF8StringEncoding: UInt = 8

// This extension will cause ClangImporter/newtype_conformance.swift to fail
// unless rdar://142693093 is fixed. To reproduce, it's important that this
// extension come *before* the _ObjectiveCBridgeable extension for String.
extension NSFileAttributeKey { }

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

extension NSError : Error, UnsafeSendable { // expected-warning{{'UnsafeSendable' is deprecated: Use @unchecked Sendable instead}}
  public var _domain: String { return domain }
  public var _code: Int { return code }
}

internal enum _GenericObjCError : Error {
  case nilError
}

public func _convertNSErrorToError(_ error: NSError?) -> Error {
  if let error = error {
    return error
  }
  return _GenericObjCError.nilError
}

public func _convertErrorToNSError(_ x: Error) -> NSError {
  return x as NSError
}

extension _SwiftNewtypeWrapper where Self.RawValue == Error {
  @inlinable // FIXME(sil-serialize-all)
  public func _bridgeToObjectiveC() -> NSError {
    return rawValue as NSError
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func _forceBridgeFromObjectiveC(
    _ source: NSError,
    result: inout Self?
  ) {
    result = Self(rawValue: source)
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func _conditionallyBridgeFromObjectiveC(
    _ source: NSError,
    result: inout Self?
  ) -> Bool {
    result = Self(rawValue: source)
    return result != nil
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSError?
  ) -> Self {
    return Self(rawValue: _convertNSErrorToError(source))!
  }
}



extension NSArray {
  @objc(methodIntroducedInOverlay) public func introducedInOverlay() { }
}

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
  init?(_bridgedNSError error: NSError) {
    self.init(_nsError: error)
  }
}

/// Various helper implementations for _BridgedStoredNSError
public extension _BridgedStoredNSError
    where Code: RawRepresentable, Code.RawValue: SignedInteger {
  // FIXME: Generalize to Integer.
  var code: Code {
    return Code(rawValue: numericCast(_nsError.code))!
  }

  /// Initialize an error within this domain with the given ``code``
  /// and ``userInfo``.
  init(_ code: Code, userInfo: [String : Any] = [:]) {
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
  var code: Code {
    return Code(rawValue: numericCast(_nsError.code))!
  }

  /// Initialize an error within this domain with the given ``code``
  /// and ``userInfo``.
  init(_ code: Code, userInfo: [String : Any] = [:]) {
    self.init(_nsError: NSError(domain: "", code: 0, userInfo: [:]))
  }
}

extension NSDictionary {
  @objc public subscript(_: Any) -> Any? {
    @objc(__swift_objectForKeyedSubscript:)
    get { fatalError() }
  }

  public func nonObjCExtensionMethod<T>(_: T) {}
}
extension NSMutableDictionary {
  public override subscript(_: Any) -> Any? {
    get { fatalError() }
    @objc(__swift_setObject:forKeyedSubscript:)
    set { }
  }
}
