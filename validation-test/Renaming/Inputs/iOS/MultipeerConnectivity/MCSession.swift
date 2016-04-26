
@available(iOS 7.0, *)
enum MCSessionSendDataMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case reliable
  case unreliable
}
@available(iOS 7.0, *)
enum MCSessionState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notConnected
  case connecting
  case connected
}
@available(iOS 7.0, *)
enum MCEncryptionPreference : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case optional
  case required
  case none
}
@available(iOS 7.0, *)
let kMCSessionMinimumNumberOfPeers: Int
@available(iOS 7.0, *)
let kMCSessionMaximumNumberOfPeers: Int
@available(iOS 7.0, *)
class MCSession : NSObject {
  convenience init(peer myPeerID: MCPeerID)
  init(peer myPeerID: MCPeerID, securityIdentity identity: [AnyObject]?, encryptionPreference encryptionPreference: MCEncryptionPreference)
  func send(_ data: NSData, toPeers peerIDs: [MCPeerID], with mode: MCSessionSendDataMode) throws
  func disconnect()
  @discardableResult
  func sendResource(at resourceURL: NSURL, withName resourceName: String, toPeer peerID: MCPeerID, withCompletionHandler completionHandler: ((NSError?) -> Void)? = nil) -> NSProgress?
  @discardableResult
  func startStream(withName streamName: String, toPeer peerID: MCPeerID) throws -> NSOutputStream
  weak var delegate: @sil_weak MCSessionDelegate?
  var myPeerID: MCPeerID { get }
  var securityIdentity: [AnyObject]? { get }
  var encryptionPreference: MCEncryptionPreference { get }
  var connectedPeers: [MCPeerID] { get }
}
protocol MCSessionDelegate : NSObjectProtocol {
  @available(iOS 7.0, *)
  func session(_ session: MCSession, peer peerID: MCPeerID, didChange state: MCSessionState)
  @available(iOS 7.0, *)
  func session(_ session: MCSession, didReceive data: NSData, fromPeer peerID: MCPeerID)
  @available(iOS 7.0, *)
  func session(_ session: MCSession, didReceive stream: NSInputStream, withName streamName: String, fromPeer peerID: MCPeerID)
  @available(iOS 7.0, *)
  func session(_ session: MCSession, didStartReceivingResourceWithName resourceName: String, fromPeer peerID: MCPeerID, with progress: NSProgress)
  @available(iOS 7.0, *)
  func session(_ session: MCSession, didFinishReceivingResourceWithName resourceName: String, fromPeer peerID: MCPeerID, at localURL: NSURL, withError error: NSError?)
  @available(iOS 7.0, *)
  optional func session(_ session: MCSession, didReceiveCertificate certificate: [AnyObject]?, fromPeer peerID: MCPeerID, certificateHandler certificateHandler: (Bool) -> Void)
}
extension MCSession {
  func nearbyConnectionData(forPeer peerID: MCPeerID, withCompletionHandler completionHandler: (NSData, NSError?) -> Void)
  func connectPeer(_ peerID: MCPeerID, withNearbyConnectionData data: NSData)
  func cancelConnectPeer(_ peerID: MCPeerID)
}
