
@available(OSX 10.11, *)
class NEPacketTunnelNetworkSettings : NETunnelNetworkSettings {
  @available(OSX 10.11, *)
  @NSCopying var iPv4Settings: NEIPv4Settings?
  @available(OSX 10.11, *)
  @NSCopying var iPv6Settings: NEIPv6Settings?
  @available(OSX 10.11, *)
  @NSCopying var tunnelOverheadBytes: NSNumber?
  @available(OSX 10.11, *)
  @NSCopying var mtu: NSNumber?
}
