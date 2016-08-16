// This is an overlay Swift module.
@_exported import Foundation

@_exported import ObjectiveC

// String/NSString bridging functions.
@_silgen_name("swift_StringToNSString") internal
func _convertStringToNSString(_ string: String) -> NSString

@_silgen_name("swift_NSStringToString") internal
func _convertNSStringToString(_ nsstring: NSString?) -> String

@_silgen_name("swift_ArrayToNSArray") internal
func _convertArrayToNSArray<T>(array: Array<T>) -> NSArray

@_silgen_name("swift_NSArrayToArray") internal
func _convertNSArrayToArray<T>(nsstring: NSArray?) -> Array<T>

@_silgen_name("swift_DictionaryToNSDictionary") internal
func _convertDictionaryToNSDictionary<K: Hashable, V>(array: Dictionary<K, V>) -> NSDictionary

@_silgen_name("swift_NSDictionaryToDictionary") internal
func _convertNSDictionaryToDictionary<K: Hashable, V>(nsstring: NSDictionary?) -> Dictionary<K, V>

// NSSet bridging entry points
func _convertSetToNSSet<T: Hashable>(s: Set<T>) -> NSSet {
  return NSSet()
}

func _convertNSSetToSet<T: NSObject>(s: NSSet?) -> Set<T> {
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

extension NSError: Error {
  public var _domain: String { return domain }
  public var _code: Int { return code }
}

public func _convertErrorToNSError(_ x: Error) -> NSError {
  return x as NSError
}
