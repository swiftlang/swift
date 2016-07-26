
@available(iOS 9.0, *)
class NEPacketTunnelFlow : NSObject {
  @available(iOS 9.0, *)
  func readPackets(completionHandler completionHandler: ([NSData], [NSNumber]) -> Void)
  @available(iOS 9.0, *)
  @discardableResult
  func writePackets(_ packets: [NSData], withProtocols protocols: [NSNumber]) -> Bool
}
