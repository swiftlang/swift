
enum GKMatchSendDataMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case reliable
  case unreliable
}
enum GKPlayerConnectionState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case stateUnknown
  case stateConnected
  case stateDisconnected
}
@available(tvOS 4.1, *)
class GKMatch : NSObject {
  @available(tvOS 8.0, *)
  var players: [GKPlayer] { get }
  unowned(unsafe) var delegate: @sil_unmanaged GKMatchDelegate?
  var expectedPlayerCount: Int { get }
  @available(tvOS 8.0, *)
  func send(_ data: NSData, to players: [GKPlayer], dataMode mode: GKMatchSendDataMode) throws
  func sendData(toAllPlayers data: NSData, with mode: GKMatchSendDataMode) throws
  func disconnect()
  @discardableResult
  func voiceChat(withName name: String) -> GKVoiceChat?
  @available(tvOS 8.0, *)
  func chooseBestHostingPlayer(completionHandler completionHandler: (GKPlayer?) -> Void)
  @available(tvOS 6.0, *)
  func rematch(completionHandler completionHandler: ((GKMatch?, NSError?) -> Void)? = nil)
}
protocol GKMatchDelegate : NSObjectProtocol {
  @available(tvOS 8.0, *)
  optional func match(_ match: GKMatch, didReceive data: NSData, fromRemotePlayer player: GKPlayer)
  @available(tvOS 9.0, *)
  optional func match(_ match: GKMatch, didReceive data: NSData, forRecipient recipient: GKPlayer, fromRemotePlayer player: GKPlayer)
  @available(tvOS 4.1, *)
  optional func match(_ match: GKMatch, player player: GKPlayer, didChange state: GKPlayerConnectionState)
  @available(tvOS 4.1, *)
  optional func match(_ match: GKMatch, didFailWithError error: NSError?)
  @available(tvOS 8.0, *)
  @discardableResult
  optional func match(_ match: GKMatch, shouldReinviteDisconnectedPlayer player: GKPlayer) -> Bool
}
extension GKMatch {
}
