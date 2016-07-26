
let NSGlobalDomain: String
let NSArgumentDomain: String
let NSRegistrationDomain: String
class NSUserDefaults : NSObject {
  @discardableResult
  class func standard() -> NSUserDefaults
  class func resetStandardUserDefaults()
  @available(tvOS 7.0, *)
  init?(suiteName suitename: String?)
  @discardableResult
  func object(forKey defaultName: String) -> AnyObject?
  func set(_ value: AnyObject?, forKey defaultName: String)
  func removeObject(forKey defaultName: String)
  @discardableResult
  func string(forKey defaultName: String) -> String?
  @discardableResult
  func array(forKey defaultName: String) -> [AnyObject]?
  @discardableResult
  func dictionary(forKey defaultName: String) -> [String : AnyObject]?
  @discardableResult
  func data(forKey defaultName: String) -> NSData?
  @discardableResult
  func stringArray(forKey defaultName: String) -> [String]?
  @discardableResult
  func integer(forKey defaultName: String) -> Int
  @discardableResult
  func float(forKey defaultName: String) -> Float
  @discardableResult
  func double(forKey defaultName: String) -> Double
  @discardableResult
  func bool(forKey defaultName: String) -> Bool
  @available(tvOS 4.0, *)
  @discardableResult
  func url(forKey defaultName: String) -> NSURL?
  func set(_ value: Int, forKey defaultName: String)
  func set(_ value: Float, forKey defaultName: String)
  func set(_ value: Double, forKey defaultName: String)
  func set(_ value: Bool, forKey defaultName: String)
  @available(tvOS 4.0, *)
  func setURL(_ url: NSURL?, forKey defaultName: String)
  func register(_ registrationDictionary: [String : AnyObject])
  func addSuite(named suiteName: String)
  func removeSuite(named suiteName: String)
  @discardableResult
  func dictionaryRepresentation() -> [String : AnyObject]
  var volatileDomainNames: [String] { get }
  @discardableResult
  func volatileDomain(forName domainName: String) -> [String : AnyObject]
  func setVolatileDomain(_ domain: [String : AnyObject], forName domainName: String)
  func removeVolatileDomain(forName domainName: String)
  @discardableResult
  func persistentDomain(forName domainName: String) -> [String : AnyObject]?
  func setPersistentDomain(_ domain: [String : AnyObject], forName domainName: String)
  func removePersistentDomain(forName domainName: String)
  @discardableResult
  func synchronize() -> Bool
  @discardableResult
  func objectIsForced(forKey key: String) -> Bool
  @discardableResult
  func objectIsForced(forKey key: String, inDomain domain: String) -> Bool
}
@available(tvOS 9.3, *)
let NSUserDefaultsSizeLimitExceededNotification: String
@available(tvOS 9.3, *)
let NSUbiquitousUserDefaultsDidChangeAccountsNotification: String
@available(tvOS 9.3, *)
let NSUbiquitousUserDefaultsCompletedInitialSyncNotification: String
let NSUserDefaultsDidChangeNotification: String
