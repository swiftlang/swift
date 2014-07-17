// Fake Foundation module for testing bridging.

@exported import ObjectiveC
@exported import Foundation // clang module

@asmname("swift_StringToNSString")
func _convertStringToNSString(string: String) -> NSString

@asmname("swift_NSStringToString")
func _convertNSStringToString(nsstring: NSString) -> String

// NSArray bridging entry points
func _convertNSArrayToArray<T>(nsarr: NSArray) -> [T] {
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
       d: NSDictionary
     ) -> Dictionary<K, V> {
  return Dictionary<K, V>()
}

extension String : _BridgedToObjectiveCType {
  public static func _getObjectiveCType() -> Any.Type {
    return NSString.self
  }
  public func _bridgeToObjectiveC() -> NSString {
    return NSString()
  }
  public static func _bridgeFromObjectiveC(x: NSString) -> String {
    fatal("implement")
  }
}

extension Int : _BridgedToObjectiveCType {
  public static func _getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }
  public func _bridgeToObjectiveC() -> NSNumber {
    return NSNumber()
  }
  public static func _bridgeFromObjectiveC(x: NSNumber) -> Int {
    fatal("implement")
  }
}

extension Array : _ConditionallyBridgedToObjectiveCType {
  public static func _getObjectiveCType() -> Any.Type {
    return NSArray.self
  }
  public func _bridgeToObjectiveC() -> NSArray {
    return NSArray()
  }
  public static func _bridgeFromObjectiveC(x: NSArray) -> Array {
    fatal("implement")
  }
  static func _bridgeFromObjectiveCConditional(x: NSArray) -> Array? {
    return nil
  }
  static func _isBridgedToObjectiveC() -> Bool {
    return Swift._isBridgedToObjectiveC(T.self)
  }
}

extension Dictionary : _ConditionallyBridgedToObjectiveCType {
  public static func _getObjectiveCType() -> Any.Type {
    return NSDictionary.self
  }
  public func _bridgeToObjectiveC() -> NSDictionary {
    return NSDictionary()
  }
  public static func _bridgeFromObjectiveC(x: NSDictionary) -> Dictionary {
    fatal("implement")
  }
  static func _bridgeFromObjectiveCConditional(x: NSDictionary) -> Dictionary? {
    return nil
  }
  static func _isBridgedToObjectiveC() -> Bool {
    return Swift._isBridgedToObjectiveC(T.self)
  }
}

extension NSObject : Hashable {
  public var hashValue: Int { return 0 }
}

public func == (x: NSObject, y: NSObject) -> Bool { return true }
