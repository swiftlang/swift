
@available(iOS 8.0, *)
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
@available(iOS 8.0, *)
let NEVPNErrorDomain: String
@available(iOS 8.0, *)
let NEVPNConfigurationChangeNotification: String
@available(iOS 8.0, *)
class NEVPNManager : NSObject {
  @available(iOS 8.0, *)
  @discardableResult
  class func shared() -> NEVPNManager
  @available(iOS 8.0, *)
  func loadFromPreferences(completionHandler completionHandler: (NSError?) -> Void)
  @available(iOS 8.0, *)
  func removeFromPreferences(completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  @available(iOS 8.0, *)
  func saveToPreferences(completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  @available(iOS 8.0, *)
  var onDemandRules: [NEOnDemandRule]?
  @available(iOS 8.0, *)
  var isOnDemandEnabled: Bool
  @available(iOS 8.0, *)
  var localizedDescription: String?
  @available(iOS, introduced: 8.0, deprecated: 9.0, message: "Use protocolConfiguration instead")
  var `protocol`: NEVPNProtocol?
  @available(iOS 9.0, *)
  var protocolConfiguration: NEVPNProtocol?
  @available(iOS 8.0, *)
  var connection: NEVPNConnection { get }
  @available(iOS 8.0, *)
  var isEnabled: Bool
}
