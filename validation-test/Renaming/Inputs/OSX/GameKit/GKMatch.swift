
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
@available(OSX 10.8, *)
class GKMatch : NSObject {
  @available(OSX 10.10, *)
  var players: [GKPlayer] { get }
  unowned(unsafe) var delegate: @sil_unmanaged GKMatchDelegate?
  var expectedPlayerCount: Int { get }
  @available(OSX 10.10, *)
  func send(_ data: NSData, to players: [GKPlayer], dataMode mode: GKMatchSendDataMode) throws
  func sendData(toAllPlayers data: NSData, with mode: GKMatchSendDataMode) throws
  func disconnect()
  @discardableResult
  func voiceChat(withName name: String) -> GKVoiceChat?
  @available(OSX 10.10, *)
  func chooseBestHostingPlayer(completionHandler completionHandler: (GKPlayer?) -> Void)
  @available(OSX 10.9, *)
  func rematch(completionHandler completionHandler: ((GKMatch?, NSError?) -> Void)? = nil)
}
protocol GKMatchDelegate : NSObjectProtocol {
  @available(OSX 10.10, *)
  optional func match(_ match: GKMatch, didReceive data: NSData, fromRemotePlayer player: GKPlayer)
  @available(OSX 10.11, *)
  optional func match(_ match: GKMatch, didReceive data: NSData, forRecipient recipient: GKPlayer, fromRemotePlayer player: GKPlayer)
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "use match:didReceiveData:fromRemotePlayer:")
  optional func match(_ match: GKMatch, didReceive data: NSData, fromPlayer playerID: String)
  @available(OSX 10.8, *)
  optional func match(_ match: GKMatch, player player: GKPlayer, didChange state: GKPlayerConnectionState)
  @available(OSX 10.8, *)
  optional func match(_ match: GKMatch, didFailWithError error: NSError?)
  @available(OSX 10.10, *)
  @discardableResult
  optional func match(_ match: GKMatch, shouldReinviteDisconnectedPlayer player: GKPlayer) -> Bool
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "use shouldReinviteDisconnectedPlayer:")
  @discardableResult
  optional func match(_ match: GKMatch, shouldReinvitePlayer playerID: String) -> Bool
}
extension GKMatch {
  @available(OSX, introduced: 10.9, deprecated: 10.10, message: "use chooseBestHostingPlayerWithCompletionHandler:")
  func chooseBestHostPlayer(completionHandler completionHandler: (String?) -> Void)
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "use sendData:toPlayers:dataMode:error:")
  func send(_ data: NSData, toPlayers playerIDs: [String], with mode: GKMatchSendDataMode) throws
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "use players")
  var playerIDs: [String] { get }
}
