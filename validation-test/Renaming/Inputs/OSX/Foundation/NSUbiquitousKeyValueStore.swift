
@available(OSX 10.7, *)
class NSUbiquitousKeyValueStore : NSObject {
  @discardableResult
  class func defaultStore() -> NSUbiquitousKeyValueStore
  @discardableResult
  func object(forKey aKey: String) -> AnyObject?
  func set(_ anObject: AnyObject?, forKey aKey: String)
  func removeObject(forKey aKey: String)
  @discardableResult
  func string(forKey aKey: String) -> String?
  @discardableResult
  func array(forKey aKey: String) -> [AnyObject]?
  @discardableResult
  func dictionary(forKey aKey: String) -> [String : AnyObject]?
  @discardableResult
  func data(forKey aKey: String) -> NSData?
  @discardableResult
  func longLong(forKey aKey: String) -> Int64
  @discardableResult
  func double(forKey aKey: String) -> Double
  @discardableResult
  func bool(forKey aKey: String) -> Bool
  func set(_ aString: String?, forKey aKey: String)
  func set(_ aData: NSData?, forKey aKey: String)
  func set(_ anArray: [AnyObject]?, forKey aKey: String)
  func set(_ aDictionary: [String : AnyObject]?, forKey aKey: String)
  func set(_ value: Int64, forKey aKey: String)
  func set(_ value: Double, forKey aKey: String)
  func set(_ value: Bool, forKey aKey: String)
  var dictionaryRepresentation: [String : AnyObject] { get }
  @discardableResult
  func synchronize() -> Bool
}
@available(OSX 10.7, *)
let NSUbiquitousKeyValueStoreDidChangeExternallyNotification: String
@available(OSX 10.7, *)
let NSUbiquitousKeyValueStoreChangeReasonKey: String
@available(OSX 10.7, *)
let NSUbiquitousKeyValueStoreChangedKeysKey: String
@available(OSX 10.7, *)
var NSUbiquitousKeyValueStoreServerChange: Int { get }
@available(OSX 10.7, *)
var NSUbiquitousKeyValueStoreInitialSyncChange: Int { get }
@available(OSX 10.7, *)
var NSUbiquitousKeyValueStoreQuotaViolationChange: Int { get }
@available(OSX 10.8, *)
var NSUbiquitousKeyValueStoreAccountChange: Int { get }
