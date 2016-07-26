
@available(iOS 9.0, *)
class NEPacketTunnelNetworkSettings : NETunnelNetworkSettings {
  @available(iOS 9.0, *)
  @NSCopying var iPv4Settings: NEIPv4Settings?
  @available(iOS 9.0, *)
  @NSCopying var iPv6Settings: NEIPv6Settings?
  @available(iOS 9.0, *)
  @NSCopying var tunnelOverheadBytes: NSNumber?
  @available(iOS 9.0, *)
  @NSCopying var mtu: NSNumber?
}
