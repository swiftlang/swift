
@available(OSX 10.11, *)
class NETunnelProviderManager : NEVPNManager {
  @available(OSX 10.11, *)
  class func loadAllFromPreferences(completionHandler completionHandler: ([NETunnelProviderManager]?, NSError?) -> Void)
  @available(OSX 10.11, *)
  @discardableResult
  func copyAppRules() -> [NEAppRule]?
  @available(OSX 10.11, *)
  var routingMethod: NETunnelProviderRoutingMethod { get }
}
