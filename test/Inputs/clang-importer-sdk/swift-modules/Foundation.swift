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
internal func _convertDictionaryToNSDictionary<KeyType, ValueType>(
    d: Dictionary<KeyType, ValueType>
) -> NSDictionary {
  return NSDictionary()
}

internal func _convertNSDictionaryToDictionary<K: NSObject, V: AnyObject>(
       d: NSDictionary
     ) -> Dictionary<K, V> {
  return Dictionary<K, V>()
}

extension String : _BridgedToObjectiveC {
  public static func getObjectiveCType() -> Any.Type {
    return NSString.self
  }
  public func bridgeToObjectiveC() -> NSString {
    return NSString()
  }
  public static func bridgeFromObjectiveC(x: NSString) -> String {
    _fatalError("implement")
  }
}

extension Int : _BridgedToObjectiveC {
  public static func getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }
  public func bridgeToObjectiveC() -> NSNumber {
    return NSNumber()
  }
  public static func bridgeFromObjectiveC(x: NSNumber) -> Int {
    _fatalError("implement")
  }
}

extension Array : _BridgedToObjectiveC {
  public static func getObjectiveCType() -> Any.Type {
    return NSArray.self
  }
  public func bridgeToObjectiveC() -> NSArray {
    return NSArray()
  }
  public static func bridgeFromObjectiveC(x: NSArray) -> Array {
    return []
  }
}

extension Dictionary : _BridgedToObjectiveC {
  public static func getObjectiveCType() -> Any.Type {
    return NSDictionary.self
  }
  public func bridgeToObjectiveC() -> NSDictionary {
    return NSDictionary()
  }
  public static func bridgeFromObjectiveC(x: NSDictionary) -> Dictionary {
    return [:]
  }
}

extension CGFloat : _BridgedToObjectiveC {
  public static func getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }
  public func bridgeToObjectiveC() -> NSNumber {
    return NSNumber()
  }
  public static func bridgeFromObjectiveC(x: NSNumber) -> CGFloat {
    return CGFloat()
  }
}
