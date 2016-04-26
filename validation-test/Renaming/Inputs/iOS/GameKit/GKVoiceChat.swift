
enum GKVoiceChatPlayerState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case connected
  case disconnected
  case speaking
  case silent
  case connecting
}
@available(iOS 4.1, *)
class GKVoiceChat : NSObject {
  func start()
  func stop()
  @available(iOS 8.0, *)
  func setPlayer(_ player: GKPlayer, muted isMuted: Bool)
  @available(iOS 8.0, *)
  var playerVoiceChatStateDidChangeHandler: (GKPlayer, GKVoiceChatPlayerState) -> Void
  var name: String { get }
  var isActive: Bool
  var volume: Float
  @available(iOS 8.0, *)
  var players: [GKPlayer] { get }
  @discardableResult
  class func isVoIPAllowed() -> Bool
}
extension GKVoiceChat {
  @available(iOS, introduced: 5.0, deprecated: 8.0, message: "use players")
  var playerIDs: [String] { get }
  @available(iOS, introduced: 4.1, deprecated: 8.0, message: "use setPlayerVoiceChatStateDidChangeHandler:")
  var playerStateUpdateHandler: (String, GKVoiceChatPlayerState) -> Void
  @available(iOS, introduced: 5.0, deprecated: 8.0, message: "use setPlayer:muted:")
  func setMute(_ isMuted: Bool, forPlayer playerID: String)
}
