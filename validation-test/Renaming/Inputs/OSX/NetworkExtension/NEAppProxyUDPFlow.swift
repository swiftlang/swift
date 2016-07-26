
@available(OSX 10.11, *)
class NEAppProxyUDPFlow : NEAppProxyFlow {
  @available(OSX 10.11, *)
  func readDatagrams(completionHandler completionHandler: ([NSData]?, [NWEndpoint]?, NSError?) -> Void)
  @available(OSX 10.11, *)
  func writeDatagrams(_ datagrams: [NSData], sentBy remoteEndpoints: [NWEndpoint], completionHandler completionHandler: (NSError?) -> Void)
  @available(OSX 10.11, *)
  var localEndpoint: NWEndpoint? { get }
}
