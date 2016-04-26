
@available(iOS 9.0, *)
class NEPacketTunnelProvider : NETunnelProvider {
  @available(iOS 9.0, *)
  func startTunnel(options options: [String : NSObject]? = [:], completionHandler completionHandler: (NSError?) -> Void)
  @available(iOS 9.0, *)
  func stopTunnel(with reason: NEProviderStopReason, completionHandler completionHandler: () -> Void)
  @available(iOS 9.0, *)
  func cancelTunnelWithError(_ error: NSError?)
  @available(iOS 9.0, *)
  var packetFlow: NEPacketTunnelFlow { get }
  @available(iOS 9.0, *)
  @discardableResult
  func createTCPConnectionThroughTunnel(to remoteEndpoint: NWEndpoint, enableTLS enableTLS: Bool, tlsParameters TLSParameters: NWTLSParameters?, delegate delegate: AnyObject?) -> NWTCPConnection
  @available(iOS 9.0, *)
  @discardableResult
  func createUDPSessionThroughTunnel(to remoteEndpoint: NWEndpoint, from localEndpoint: NWHostEndpoint?) -> NWUDPSession
}
