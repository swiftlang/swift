@exported import ObjectiveC
@exported import CoreGraphics
@exported import Foundation

@asmname("swift_StringToNSString") internal
func _convertStringToNSString(string: String) -> NSString

@asmname("swift_NSStringToString") internal
func _convertNSStringToString(nsstring: NSString) -> String

public struct NSZone: NilLiteralConvertible {
  public var pointer : COpaquePointer

  @transparent public
  static func convertFromNilLiteral() -> NSZone {
    return NSZone(pointer: COpaquePointer())
  }
}

public func == (lhs: NSObject, rhs: NSObject) -> Bool {
  return lhs.isEqual(rhs)
}

public let NSUTF8StringEncoding: UInt = 8

// NSArray bridging entry points
func _convertNSArrayToArray<T>(nsarr: NSArray) -> [T] {
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
       d: NSDictionary
     ) -> Dictionary<K, V> {
  return Dictionary<K, V>()
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
  public static func _forceBridgeFromObjectiveC(x: NSString) -> String {
    _fatalError("implement")
  }
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSString
  ) -> String? {
    return self._forceBridgeFromObjectiveC(x)
  }
}

extension Int : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public static func _getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }
  public func _bridgeToObjectiveC() -> NSNumber {
    return NSNumber()
  }
  public static func _forceBridgeFromObjectiveC(x: NSNumber) -> Int {
    _fatalError("implement")
  }
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSNumber
  ) -> Int? {
    return self._forceBridgeFromObjectiveC(x)
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
  public static func _forceBridgeFromObjectiveC(x: NSArray) -> Array {
    return []
  }
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSArray
  ) -> Array? {
    return self._forceBridgeFromObjectiveC(x)
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
  public static func _forceBridgeFromObjectiveC(x: NSDictionary) -> Dictionary {
    return [:]
  }
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSDictionary
  ) -> Dictionary? {
    return self._forceBridgeFromObjectiveC(x)
  }
}

extension CGFloat : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public static func _getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }
  public func _bridgeToObjectiveC() -> NSNumber {
    return NSNumber()
  }
  public static func _forceBridgeFromObjectiveC(x: NSNumber) -> CGFloat {
    return CGFloat()
  }
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSNumber
  ) -> CGFloat? {
    return self._forceBridgeFromObjectiveC(x)
  }
}
