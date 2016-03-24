@_exported import ObjectiveC
@_exported import CoreGraphics
@_exported import Foundation

@_silgen_name("swift_StringToNSString") internal
func _convertStringToNSString(string: String) -> NSString

@_silgen_name("swift_NSStringToString") internal
func _convertNSStringToString(nsstring: NSString?) -> String

public func == (lhs: NSObject, rhs: NSObject) -> Bool {
  return lhs.isEqual(rhs)
}

public let NSUTF8StringEncoding: UInt = 8

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

internal func _convertNSDictionaryToDictionary<K: NSObject, V: AnyObject>(
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
  public static func _unconditionallyBridgeFromObjectiveC(x: NSString?) -> String {
    return String()
 }
}

extension Int : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public func _bridgeToObjectiveC() -> NSNumber {
    return NSNumber()
  }
  public static func _forceBridgeFromObjectiveC(
    x: NSNumber, 
    result: inout Int?
  ) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSNumber,
    result: inout Int?
  ) -> Bool {
    return true
  }
  public static func _unconditionallyBridgeFromObjectiveC(
    x: NSNumber?
  ) -> Int {
    return 0
  }
}

extension Array : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
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
  public static func _unconditionallyBridgeFromObjectiveC(
    x: NSArray?
  ) -> Array {
    return Array()
  }
}

extension Dictionary : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
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
  public static func _unconditionallyBridgeFromObjectiveC(
    x: NSDictionary?
  ) -> Dictionary {
    return Dictionary()
  }
}

extension Set : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
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
  public static func _unconditionallyBridgeFromObjectiveC(
    x: NSSet?
  ) -> Set {
    return Set()
  }
}

extension CGFloat : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public func _bridgeToObjectiveC() -> NSNumber {
    return NSNumber()
  }
  public static func _forceBridgeFromObjectiveC(
    x: NSNumber,
    result: inout CGFloat?
  ) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSNumber,
    result: inout CGFloat?
  ) -> Bool {
    return true
  }
  public static func _unconditionallyBridgeFromObjectiveC(
    x: NSNumber?
  ) -> CGFloat {
    return CGFloat()
  }
}

extension NSRange : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public func _bridgeToObjectiveC() -> NSValue {
    return NSValue()
  }

  public static func _forceBridgeFromObjectiveC(
    x: NSValue,
    result: inout NSRange?
  ) {
    result = x.rangeValue
  }
  
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSValue,
    result: inout NSRange?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }

  public static func _unconditionallyBridgeFromObjectiveC(
    x: NSValue?
  ) -> NSRange {
    return NSRange()
  }
}

extension NSError : ErrorProtocol {
  public var _domain: String { return domain }
  public var _code: Int { return code }
}

extension NSArray {
  @objc(methodIntroducedInOverlay) public func introducedInOverlay() { }
}

@_silgen_name("swift_convertNSErrorToErrorProtocol")
func _convertNSErrorToErrorProtocol(string: NSError?) -> ErrorProtocol

@_silgen_name("swift_convertErrorProtocolToNSError")
func _convertErrorProtocolToNSError(string: ErrorProtocol) -> NSError
