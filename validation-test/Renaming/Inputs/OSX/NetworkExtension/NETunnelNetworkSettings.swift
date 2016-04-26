
@available(OSX 10.11, *)
class NETunnelNetworkSettings : NSObject, NSSecureCoding, NSCopying {
  @available(OSX 10.11, *)
  init(tunnelRemoteAddress address: String)
  @available(OSX 10.11, *)
  var tunnelRemoteAddress: String { get }
  @available(OSX 10.11, *)
  @NSCopying var dnsSettings: NEDNSSettings?
  @available(OSX 10.11, *)
  @NSCopying var proxySettings: NEProxySettings?
}
