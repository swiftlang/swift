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
func _convertDictionaryToNSDictionary<KeyType, ValueType>(
    d: Dictionary<KeyType, ValueType>
) -> NSDictionary {
  return NSDictionary()
}

func _convertNSDictionaryToDictionary<K: NSObject, V: AnyObject>(
       d: NSDictionary
     ) -> Dictionary<K, V> {
  return Dictionary<K, V>()
}

extension String : _BridgedToObjectiveC {
  static func getObjectiveCType() -> Any.Type {
    return NSString.self
  }
  func bridgeToObjectiveC() -> NSString {
    return NSString()
  }
  static func bridgeFromObjectiveC(x: NSString) -> String {
    fatal("implement")
  }
}

extension Int : _BridgedToObjectiveC {
  static func getObjectiveCType() -> Any.Type {
    return NSNumber.self
  }
  func bridgeToObjectiveC() -> NSNumber {
    return NSNumber()
  }
  static func bridgeFromObjectiveC(x: NSNumber) -> Int {
    fatal("implement")
  }
}

extension Array : _ConditionallyBridgedToObjectiveC {
  static func getObjectiveCType() -> Any.Type {
    return NSArray.self
  }
  func bridgeToObjectiveC() -> NSArray {
    return NSArray()
  }
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

extension Dictionary : _ConditionallyBridgedToObjectiveC {
  static func getObjectiveCType() -> Any.Type {
    return NSDictionary.self
  }
  func bridgeToObjectiveC() -> NSDictionary {
    return NSDictionary()
  }
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
  var hashValue: Int { return 0 }
}

func == (x: NSObject, y: NSObject) -> Bool { return true }
