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
  public static func getObjectiveCType() -> Any.Type {
    return NSString.self
  }
  public func bridgeToObjectiveC() -> NSString {
    return NSString()
  }
  public static func bridgeFromObjectiveC(x: NSString) -> String {
    fatal("implement")
  }
}

extension Int : _BridgedToObjectiveCType {
  public static func getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }
  public func bridgeToObjectiveC() -> NSNumber {
    return NSNumber()
  }
  public static func bridgeFromObjectiveC(x: NSNumber) -> Int {
    fatal("implement")
  }
}

extension Array : _ConditionallyBridgedToObjectiveCType {
  public
  static func getObjectiveCType() -> Any.Type {
    return NSArray.self
  }
  public
  func bridgeToObjectiveC() -> NSArray {
    return NSArray()
  }
  public
  static func bridgeFromObjectiveC(x: NSArray) -> Array {
    fatal("implement")
  }
  static func bridgeFromObjectiveCConditional(x: NSArray) -> Array? {
    return nil
  }
  static func isBridgedToObjectiveC() -> Bool {
    return Swift.isBridgedToObjectiveC(T.self)
  }
}

extension Dictionary : _ConditionallyBridgedToObjectiveCType {
  public
  static func getObjectiveCType() -> Any.Type {
    return NSDictionary.self
  }
  public
  func bridgeToObjectiveC() -> NSDictionary {
    return NSDictionary()
  }
  public
  static func bridgeFromObjectiveC(x: NSDictionary) -> Dictionary {
    fatal("implement")
  }
  static func bridgeFromObjectiveCConditional(x: NSDictionary) -> Dictionary? {
    return nil
  }
  static func isBridgedToObjectiveC() -> Bool {
    return Swift.isBridgedToObjectiveC(T.self)
  }
}

extension NSObject : Hashable {
  public var hashValue: Int { return 0 }
}

public func == (x: NSObject, y: NSObject) -> Bool { return true }
