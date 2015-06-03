// This is an overlay Swift module.
@exported import Foundation

@exported import ObjectiveC

// String/NSString bridging functions.
@asmname("swift_StringToNSString") internal
func _convertStringToNSString(string: String) -> NSString

@asmname("swift_NSStringToString") internal
func _convertNSStringToString(nsstring: NSString?) -> String

@asmname("swift_ArrayToNSArray") internal
func _convertArrayToNSArray<T>(array: Array<T>) -> NSArray

@asmname("swift_NSArrayToArray") internal
func _convertNSArrayToArray<T>(nsstring: NSArray?) -> Array<T>

@asmname("swift_DictionaryToNSDictionary") internal
func _convertDictionaryToNSDictionary<K: Hashable, V>(array: Dictionary<K, V>) -> NSDictionary

@asmname("swift_NSDictionaryToDictionary") internal
func _convertNSDictionaryToDictionary<K: Hashable, V>(nsstring: NSDictionary?) -> Dictionary<K, V>

// NSSet bridging entry points
func _convertSetToNSSet<T: Hashable>(s: Set<T>) -> NSSet {
  return NSSet()
}

func _convertNSSetToSet<T: NSObject>(s: NSSet?) -> Set<T> {
  return Set<T>()
}

extension NSError: ErrorType {
  public var _domain: String { return domain }
  public var _code: Int { return code }
}

public func _convertErrorTypeToNSError(x: ErrorType) -> NSError {
  return x as NSError
}
