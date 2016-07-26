
@available(iOS 9.0, *)
enum NEProviderStopReason : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case userInitiated
  case providerFailed
  case noNetworkAvailable
  case unrecoverableNetworkChange
  case providerDisabled
  case authenticationCanceled
  case configurationFailed
  case idleTimeout
  case configurationDisabled
  case configurationRemoved
  case superceded
  case userLogout
  case userSwitch
  case connectionFailed
}
@available(iOS 9.0, *)
class NEProvider : NSObject {
  @available(iOS 9.0, *)
  func sleep(completionHandler completionHandler: () -> Void)
  @available(iOS 9.0, *)
  func wake()
  @available(iOS 9.0, *)
  @discardableResult
  func createTCPConnection(to remoteEndpoint: NWEndpoint, enableTLS enableTLS: Bool, tlsParameters TLSParameters: NWTLSParameters?, delegate delegate: AnyObject?) -> NWTCPConnection
  @available(iOS 9.0, *)
  @discardableResult
  func createUDPSession(to remoteEndpoint: NWEndpoint, from localEndpoint: NWHostEndpoint?) -> NWUDPSession
  @available(iOS 9.0, *)
  var defaultPath: NWPath? { get }
}
