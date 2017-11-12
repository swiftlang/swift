// Fake Foundation module for testing bridging.

@_exported import ObjectiveC
@_exported import Foundation // clang module

@_silgen_name("swift_StringToNSString")
func _convertStringToNSString(string: String) -> NSString

@_silgen_name("swift_NSStringToString")
func _convertNSStringToString(nsstring: NSString?) -> String

// NSArray bridging entry points
func _convertNSArrayToArray<T>(nsarr: NSArray?) -> [T] {
  return [T]()
}

func _convertArrayToNSArray<T>(arr: [T]) -> NSArray {
  return NSArray()
}

// NSDictionary bridging entry points
func _convertDictionaryToNSDictionary<Key, Value>(
    d: Dictionary<Key, Value>
) -> NSDictionary {
  return NSDictionary()
}

func _convertNSDictionaryToDictionary<K: NSObject, V: AnyObject>(
       d: NSDictionary?
     ) -> Dictionary<K, V> {
  return Dictionary<K, V>()
}

// NSSet bridging entry points
func _convertSetToNSSet<T>(s: Set<T>) -> NSSet {
  return NSSet()
}

func _convertNSSetToSet<T>(s: NSSet?) -> Set<T> {
  return Set<T>()
}

extension String : _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> NSString {
    return NSString()
  }
  public static func _forceBridgeFromObjectiveC(
    _ x: NSString,
    result: inout String?
  ) {
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
    return 0
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

extension NSObject : Hashable {
  public var hashValue: Int { return 0 }
}

public func == (x: NSObject, y: NSObject) -> Bool { return true }

extension NSError : Error {
  public var _domain: String { return domain }
  public var _code: Int { return code }
}

extension AnyHashable : _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> NSObject {
    fatalError()
  }
  public static func _forceBridgeFromObjectiveC(
    _ x: NSObject,
    result: inout AnyHashable?
  ) {
  }
  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSObject,
    result: inout AnyHashable?
  ) -> Bool {
    fatalError()
  }
  public static func _unconditionallyBridgeFromObjectiveC(
    _ x: NSObject?
  ) -> AnyHashable {
    fatalError()
  }
}
