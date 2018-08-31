@_exported import ObjectiveC
@_exported import CoreGraphics
@_exported import Foundation

public func == (lhs: NSObject, rhs: NSObject) -> Bool {
  return lhs.isEqual(rhs)
}

public let NSUTF8StringEncoding: UInt = 8

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
  public static func _unconditionallyBridgeFromObjectiveC(
    _ x: NSString?
  ) -> String {
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
    return Int()
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

extension NSError : Error {
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

public func _convertErrorToNSError(_ error: Error) -> NSError {
  return error as NSError
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

