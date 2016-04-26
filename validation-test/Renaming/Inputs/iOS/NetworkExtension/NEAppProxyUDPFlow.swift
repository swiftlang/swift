
@available(iOS 9.0, *)
class NEAppProxyUDPFlow : NEAppProxyFlow {
  @available(iOS 9.0, *)
  func readDatagrams(completionHandler completionHandler: ([NSData]?, [NWEndpoint]?, NSError?) -> Void)
  @available(iOS 9.0, *)
  func writeDatagrams(_ datagrams: [NSData], sentBy remoteEndpoints: [NWEndpoint], completionHandler completionHandler: (NSError?) -> Void)
  @available(iOS 9.0, *)
  var localEndpoint: NWEndpoint? { get }
}
