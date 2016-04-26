
@available(iOS 9.0, *)
class NEAppProxyProvider : NETunnelProvider {
  @available(iOS 9.0, *)
  func startProxy(options options: [String : AnyObject]? = [:], completionHandler completionHandler: (NSError?) -> Void)
  @available(iOS 9.0, *)
  func stopProxy(with reason: NEProviderStopReason, completionHandler completionHandler: () -> Void)
  @available(iOS 9.0, *)
  func cancelProxyWithError(_ error: NSError?)
  @available(iOS 9.0, *)
  @discardableResult
  func handleNewFlow(_ flow: NEAppProxyFlow) -> Bool
}
