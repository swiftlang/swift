
@available(OSX 10.11, *)
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
@available(OSX 10.11, *)
class NEProvider : NSObject {
  @available(OSX 10.11, *)
  func sleep(completionHandler completionHandler: () -> Void)
  @available(OSX 10.11, *)
  func wake()
  @available(OSX 10.11, *)
  @discardableResult
  func createTCPConnection(to remoteEndpoint: NWEndpoint, enableTLS enableTLS: Bool, tlsParameters TLSParameters: NWTLSParameters?, delegate delegate: AnyObject?) -> NWTCPConnection
  @available(OSX 10.11, *)
  @discardableResult
  func createUDPSession(to remoteEndpoint: NWEndpoint, from localEndpoint: NWHostEndpoint?) -> NWUDPSession
  @available(OSX 10.11, *)
  var defaultPath: NWPath? { get }
}
