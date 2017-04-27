@_exported import ObjectiveC
@_exported import CoreGraphics
@_exported import Foundation

@_silgen_name("swift_StringToNSString")
public func _convertStringToNSString(_ string: String) -> NSString

@_silgen_name("swift_NSStringToString")
public func _convertNSStringToString(_ nsstring: NSString?) -> String

public func == (lhs: NSObject, rhs: NSObject) -> Bool {
  return lhs.isEqual(rhs)
}

public let NSUTF8StringEncoding: UInt = 8

// NSArray bridging entry points
public func _convertNSArrayToArray<T>(_ nsarr: NSArray?) -> [T] {
  return [T]()
}

public func _convertArrayToNSArray<T>(_ arr: [T]) -> NSArray {
  return NSArray()
}

// NSDictionary bridging entry points
public func _convertDictionaryToNSDictionary<Key, Value>(
    _ d: Dictionary<Key, Value>
) -> NSDictionary {
  return NSDictionary()
}

public func _convertNSDictionaryToDictionary<K: NSObject, V: AnyObject>(
       _ d: NSDictionary?
     ) -> Dictionary<K, V> {
  return Dictionary<K, V>()
}

// NSSet bridging entry points
public func _convertSetToNSSet<T>(_ s: Set<T>) -> NSSet {
  return NSSet()
}

public func _convertNSSetToSet<T>(_ s: NSSet?) -> Set<T> {
  return Set<T>()
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

@_silgen_name("swift_convertNSErrorToError")
public func _convertNSErrorToError(_ string: NSError?) -> Error

@_silgen_name("swift_convertErrorToNSError")
public func _convertErrorToNSError(_ string: Error) -> NSError
