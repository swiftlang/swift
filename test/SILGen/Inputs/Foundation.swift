// Fake Foundation module for testing bridging.

@exported import ObjectiveC
@exported import Foundation // clang module

@asmname("swift_StringToNSString")
func _convertStringToNSString(string: String) -> NSString

@asmname("swift_NSStringToString")
func _convertNSStringToString(nsstring: NSString) -> String

// NSArray bridging entry points
func _convertNSArrayToArray<T>(nsarr: NSArray) -> T[] {
  return T[]()
}

func _convertArrayToNSArray<T>(arr: T[]) -> NSArray {
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
  func bridgeToObjectiveC() -> NSString {
    return NSString()
  }
}

extension Int : _BridgedToObjectiveC {
  func bridgeToObjectiveC() -> NSNumber {
    return NSNumber()
  }
}
