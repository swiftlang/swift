
@available(iOS 9.0, *)
class NETunnelProviderSession : NEVPNConnection {
  @available(iOS 9.0, *)
  func startTunnel(options options: [String : AnyObject]? = [:]) throws
  @available(iOS 9.0, *)
  func stopTunnel()
  @available(iOS 9.0, *)
  func sendProviderMessage(_ messageData: NSData, responseHandler responseHandler: ((NSData?) -> Void)? = nil) throws
}
