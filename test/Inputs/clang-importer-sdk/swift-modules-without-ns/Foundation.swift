@_exported import ObjectiveC
@_exported import CoreGraphics
@_exported import Foundation

@_silgen_name("swift_StringToNSString") internal
func _convertStringToNSString(string: String) -> NSString

@_silgen_name("swift_NSStringToString") internal
func _convertNSStringToString(nsstring: NSString?) -> String

public func == (lhs: Object, rhs: Object) -> Bool {
  return lhs.isEqual(rhs)
}

public let utf88StringEncoding: UInt = 8

// NSArray bridging entry points
func _convertNSArrayToArray<T>(nsarr: NSArray?) -> [T] {
  return [T]()
}

func _convertArrayToNSArray<T>(arr: [T]) -> NSArray {
  return NSArray()
}

// NSDictionary bridging entry points
internal func _convertDictionaryToNSDictionary<Key, Value>(
    d: Dictionary<Key, Value>
) -> NSDictionary {
  return NSDictionary()
}

internal func _convertNSDictionaryToDictionary<K: Object, V: AnyObject>(
       d: NSDictionary?
     ) -> Dictionary<K, V> {
  return Dictionary<K, V>()
}

// NSSet bridging entry points
internal func _convertSetToNSSet<T : Hashable>(s: Set<T>) -> NSSet {
  return NSSet()
}

internal func _convertNSSetToSet<T : Hashable>(s: NSSet?) -> Set<T> {
  return Set<T>()
}

extension String : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public static func _getObjectiveCType() -> Any.Type {
    return NSString.self
  }
  public func _bridgeToObjectiveC() -> NSString {
    return NSString()
  }
  public static func _forceBridgeFromObjectiveC(x: NSString,
                                                result: inout String?) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSString,
    result: inout String?
  ) -> Bool {
    return true
  }
}

extension Int : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public static func _getObjectiveCType() -> Any.Type {
    return Number.self
  }
  public func _bridgeToObjectiveC() -> Number {
    return Number()
  }
  public static func _forceBridgeFromObjectiveC(
    x: Number, 
    result: inout Int?
  ) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    x: Number,
    result: inout Int?
  ) -> Bool {
    return true
  }
}

extension Array : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public static func _getObjectiveCType() -> Any.Type {
    return NSArray.self
  }
  public func _bridgeToObjectiveC() -> NSArray {
    return NSArray()
  }
  public static func _forceBridgeFromObjectiveC(
    x: NSArray,
    result: inout Array?
  ) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSArray,
    result: inout Array?
  ) -> Bool {
    return true
  }
}

extension Dictionary : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public static func _getObjectiveCType() -> Any.Type {
    return NSDictionary.self
  }
  public func _bridgeToObjectiveC() -> NSDictionary {
    return NSDictionary()
  }
  public static func _forceBridgeFromObjectiveC(
    x: NSDictionary,
    result: inout Dictionary?
  ) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSDictionary,
    result: inout Dictionary?
  ) -> Bool {
    return true
  }
}

extension Set : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }

  public static func _getObjectiveCType() -> Any.Type {
    return NSSet.self
  }
  public func _bridgeToObjectiveC() -> NSSet {
    return NSSet()
  }
  public static func _forceBridgeFromObjectiveC(
    x: NSSet,
    result: inout Set?
  ) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSSet,
    result: inout Set?
  ) -> Bool {
    return true
  }
}

extension CGFloat : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public static func _getObjectiveCType() -> Any.Type {
    return Number.self
  }
  public func _bridgeToObjectiveC() -> Number {
    return Number()
  }
  public static func _forceBridgeFromObjectiveC(
    x: Number,
    result: inout CGFloat?
  ) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    x: Number,
    result: inout CGFloat?
  ) -> Bool {
    return true
  }
}

extension NSRange : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public static func _getObjectiveCType() -> Any.Type {
    return Value.self
  }

  public func _bridgeToObjectiveC() -> Value {
    return Value()
  }

  public static func _forceBridgeFromObjectiveC(
    x: Value,
    result: inout NSRange?
  ) {
    result = x.rangeValue
  }
  
  public static func _conditionallyBridgeFromObjectiveC(
    x: Value,
    result: inout NSRange?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }
}

extension Error : ErrorType {
  public var _domain: String { return domain }
  public var _code: Int { return code }
}

@_silgen_name("swift_convertNSErrorToErrorType")
func _convertNSErrorToErrorType(string: Error?) -> ErrorType

@_silgen_name("swift_convertErrorTypeToNSError")
func _convertErrorTypeToNSError(string: ErrorType) -> Error
