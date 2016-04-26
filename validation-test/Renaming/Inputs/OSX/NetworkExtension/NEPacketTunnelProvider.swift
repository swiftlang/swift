
@available(OSX 10.11, *)
class NEPacketTunnelProvider : NETunnelProvider {
  @available(OSX 10.11, *)
  func startTunnel(options options: [String : NSObject]? = [:], completionHandler completionHandler: (NSError?) -> Void)
  @available(OSX 10.11, *)
  func stopTunnel(with reason: NEProviderStopReason, completionHandler completionHandler: () -> Void)
  @available(OSX 10.11, *)
  func cancelTunnelWithError(_ error: NSError?)
  @available(OSX 10.11, *)
  var packetFlow: NEPacketTunnelFlow { get }
  @available(OSX 10.11, *)
  @discardableResult
  func createTCPConnectionThroughTunnel(to remoteEndpoint: NWEndpoint, enableTLS enableTLS: Bool, tlsParameters TLSParameters: NWTLSParameters?, delegate delegate: AnyObject?) -> NWTCPConnection
  @available(OSX 10.11, *)
  @discardableResult
  func createUDPSessionThroughTunnel(to remoteEndpoint: NWEndpoint, from localEndpoint: NWHostEndpoint?) -> NWUDPSession
}
