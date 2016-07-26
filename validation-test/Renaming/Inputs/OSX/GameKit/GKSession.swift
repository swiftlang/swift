
@available(OSX, introduced: 10.8, deprecated: 10.10, message: "Use MCSession from the MultipeerConnectivity framework instead")
class GKSession : NSObject {
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  init!(sessionID sessionID: String!, displayName name: String!, sessionMode mode: GKSessionMode)
  unowned(unsafe) var delegate: @sil_unmanaged GKSessionDelegate!
  var sessionID: String! { get }
  var displayName: String! { get }
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  var sessionMode: GKSessionMode { get }
  var peerID: String! { get }
  var isAvailable: Bool
  var disconnectTimeout: NSTimeInterval
  @discardableResult
  func displayName(forPeer peerID: String!) -> String!
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  func send(_ data: NSData!, toPeers peers: [AnyObject]!, with mode: GKSendDataMode) throws
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  func sendData(toAllPeers data: NSData!, with mode: GKSendDataMode) throws
  func setDataReceiveHandler(_ handler: AnyObject!, withContext context: UnsafeMutablePointer<Void>!)
  func connect(toPeer peerID: String!, withTimeout timeout: NSTimeInterval)
  func cancelConnect(toPeer peerID: String!)
  func acceptConnection(fromPeer peerID: String!) throws
  func denyConnection(fromPeer peerID: String!)
  func disconnectPeer(fromAllPeers peerID: String!)
  func disconnectFromAllPeers()
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  @discardableResult
  func peers(with state: GKPeerConnectionState) -> [AnyObject]!
}
