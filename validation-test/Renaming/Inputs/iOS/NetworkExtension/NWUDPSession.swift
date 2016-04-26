
@available(iOS 9.0, *)
enum NWUDPSessionState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case invalid
  case waiting
  case preparing
  case ready
  case failed
  case cancelled
}
@available(iOS 9.0, *)
class NWUDPSession : NSObject {
  @available(iOS 9.0, *)
  init(upgradeFor session: NWUDPSession)
  @available(iOS 9.0, *)
  var state: NWUDPSessionState { get }
  @available(iOS 9.0, *)
  var endpoint: NWEndpoint { get }
  @available(iOS 9.0, *)
  var resolvedEndpoint: NWEndpoint? { get }
  @available(iOS 9.0, *)
  var isViable: Bool { get }
  @available(iOS 9.0, *)
  var hasBetterPath: Bool { get }
  @available(iOS 9.0, *)
  var currentPath: NWPath? { get }
  @available(iOS 9.0, *)
  func tryNextResolvedEndpoint()
  @available(iOS 9.0, *)
  var maximumDatagramLength: Int { get }
  @available(iOS 9.0, *)
  func setReadHandler(_ handler: ([NSData]?, NSError?) -> Void, maxDatagrams maxDatagrams: Int)
  @available(iOS 9.0, *)
  func writeMultipleDatagrams(_ datagramArray: [NSData], completionHandler completionHandler: (NSError?) -> Void)
  @available(iOS 9.0, *)
  func writeDatagram(_ datagram: NSData, completionHandler completionHandler: (NSError?) -> Void)
  @available(iOS 9.0, *)
  func cancel()
}
