// This is an overlay Swift module.
@_exported import Foundation

@_exported import ObjectiveC

// String/NSString bridging functions.
@_silgen_name("swift_StringToNSString") internal
func _convertStringToNSString(string: String) -> NSString

@_silgen_name("swift_NSStringToString") internal
func _convertNSStringToString(nsstring: NSString?) -> String

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

extension NSError: ErrorProtocol {
  public var _domain: String { return domain }
  public var _code: Int { return code }
}

public func _convertErrorProtocolToNSError(x: ErrorProtocol) -> NSError {
  return x as NSError
}
