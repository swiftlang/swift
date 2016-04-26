
@available(iOS 9.0, *)
class NEAppProxyTCPFlow : NEAppProxyFlow {
  @available(iOS 9.0, *)
  func readData(completionHandler completionHandler: (NSData?, NSError?) -> Void)
  @available(iOS 9.0, *)
  func write(_ data: NSData, withCompletionHandler completionHandler: (NSError?) -> Void)
  @available(iOS 9.0, *)
  var remoteEndpoint: NWEndpoint { get }
}
