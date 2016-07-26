
@available(OSX 10.11, *)
class NEAppProxyTCPFlow : NEAppProxyFlow {
  @available(OSX 10.11, *)
  func readData(completionHandler completionHandler: (NSData?, NSError?) -> Void)
  @available(OSX 10.11, *)
  func write(_ data: NSData, withCompletionHandler completionHandler: (NSError?) -> Void)
  @available(OSX 10.11, *)
  var remoteEndpoint: NWEndpoint { get }
}
