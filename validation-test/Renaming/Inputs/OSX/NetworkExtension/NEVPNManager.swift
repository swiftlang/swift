
@available(OSX 10.10, *)
enum NEVPNError : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case configurationInvalid
  case configurationDisabled
  case connectionFailed
  case configurationStale
  case configurationReadWriteFailed
  case configurationUnknown
}
@available(OSX 10.10, *)
let NEVPNErrorDomain: String
@available(OSX 10.10, *)
let NEVPNConfigurationChangeNotification: String
@available(OSX 10.10, *)
class NEVPNManager : NSObject {
  @available(OSX 10.10, *)
  @discardableResult
  class func shared() -> NEVPNManager
  @available(OSX 10.10, *)
  func loadFromPreferences(completionHandler completionHandler: (NSError?) -> Void)
  @available(OSX 10.10, *)
  func removeFromPreferences(completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  @available(OSX 10.10, *)
  func saveToPreferences(completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  @available(OSX 10.10, *)
  func setAuthorization(_ authorization: AuthorizationRef)
  @available(OSX 10.10, *)
  var onDemandRules: [NEOnDemandRule]?
  @available(OSX 10.10, *)
  var isOnDemandEnabled: Bool
  @available(OSX 10.10, *)
  var localizedDescription: String?
  @available(OSX, introduced: 10.10, deprecated: 10.11, message: "Use protocolConfiguration instead")
  var `protocol`: NEVPNProtocol?
  @available(OSX 10.11, *)
  var protocolConfiguration: NEVPNProtocol?
  @available(OSX 10.10, *)
  var connection: NEVPNConnection { get }
  @available(OSX 10.10, *)
  var isEnabled: Bool
}
