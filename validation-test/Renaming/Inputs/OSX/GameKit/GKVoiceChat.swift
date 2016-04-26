
enum GKVoiceChatPlayerState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case connected
  case disconnected
  case speaking
  case silent
  case connecting
}
@available(OSX 10.8, *)
class GKVoiceChat : NSObject {
  func start()
  func stop()
  @available(OSX 10.10, *)
  func setPlayer(_ player: GKPlayer, muted isMuted: Bool)
  @available(OSX 10.10, *)
  var playerVoiceChatStateDidChangeHandler: (GKPlayer, GKVoiceChatPlayerState) -> Void
  var name: String { get }
  var isActive: Bool
  var volume: Float
  @available(OSX 10.10, *)
  var players: [GKPlayer] { get }
  @discardableResult
  class func isVoIPAllowed() -> Bool
}
extension GKVoiceChat {
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "use players")
  var playerIDs: [String] { get }
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "use setPlayerVoiceChatStateDidChangeHandler:")
  var playerStateUpdateHandler: (String, GKVoiceChatPlayerState) -> Void
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "use setPlayer:muted:")
  func setMute(_ isMuted: Bool, forPlayer playerID: String)
}
