
@available(iOS 9.0, *)
class NETunnelProviderManager : NEVPNManager {
  @available(iOS 9.0, *)
  class func loadAllFromPreferences(completionHandler completionHandler: ([NETunnelProviderManager]?, NSError?) -> Void)
  @available(iOS 9.0, *)
  @discardableResult
  func copyAppRules() -> [NEAppRule]?
  @available(iOS 9.0, *)
  var routingMethod: NETunnelProviderRoutingMethod { get }
}
