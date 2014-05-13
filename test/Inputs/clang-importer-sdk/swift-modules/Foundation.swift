@exported import ObjectiveC
@exported import CoreGraphics
@exported import Foundation

@asmname("swift_StringToNSString")
func _convertStringToNSString(string: String) -> NSString

@asmname("swift_NSStringToString")
func _convertNSStringToString(nsstring: NSString) -> String

struct NSZone {
  var pointer : COpaquePointer
}

func == (lhs: NSObject, rhs: NSObject) -> Bool {
  return lhs.isEqual(rhs)
}

let NSUTF8StringEncoding: UInt = 8

// NSArray bridging entry points
@asmname("swift_convertNSArrayToArray")
func _convertNSArrayToArray<T>(nsarr: NSArray) -> T[]

@asmname("swift_convertArrayToNSArray")
func _convertArrayToNSArray<T>(arr: T[]) -> NSArray

// NSDictionary bridging entry points
@asmname("swift_convertDictionaryToNSDictionary")
func _convertDictionaryToNSDictionary<KeyType, ValueType>(
    d: Dictionary<KeyType, ValueType>
) -> NSDictionary

@asmname("swift_convertNSDictionaryToDictionary")
func _convertNSDictionaryToDictionary<K: NSObject, V: AnyObject>(
       d: NSDictionary
     ) -> Dictionary<K, V>

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
