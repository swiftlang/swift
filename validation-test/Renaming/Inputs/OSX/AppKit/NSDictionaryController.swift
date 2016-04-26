
@available(OSX 10.11, *)
class NSDictionaryControllerKeyValuePair : NSObject {
  var key: String?
  var value: AnyObject?
  var localizedKey: String?
  var isExplicitlyIncluded: Bool { get }
}
@available(OSX 10.5, *)
class NSDictionaryController : NSArrayController {
  var initialKey: String
  var initialValue: AnyObject
  var includedKeys: [String]
  var excludedKeys: [String]
  var localizedKeyDictionary: [String : String]
  var localizedKeyTable: String?
}
struct __dictionaryControllerFlags {
  var _deepCopiesValues: UInt32
  var _suppressBuildingDictionary: UInt32
  var _reservedDictionaryController: UInt32
  init()
  init(_deepCopiesValues _deepCopiesValues: UInt32, _suppressBuildingDictionary _suppressBuildingDictionary: UInt32, _reservedDictionaryController _reservedDictionaryController: UInt32)
}
