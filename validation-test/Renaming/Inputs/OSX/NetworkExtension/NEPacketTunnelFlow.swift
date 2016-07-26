
@available(OSX 10.11, *)
class NEPacketTunnelFlow : NSObject {
  @available(OSX 10.11, *)
  func readPackets(completionHandler completionHandler: ([NSData], [NSNumber]) -> Void)
  @available(OSX 10.11, *)
  @discardableResult
  func writePackets(_ packets: [NSData], withProtocols protocols: [NSNumber]) -> Bool
}
