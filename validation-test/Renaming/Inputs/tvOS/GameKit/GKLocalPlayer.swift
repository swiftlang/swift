
@available(tvOS 4.1, *)
class GKLocalPlayer : GKPlayer {
  @discardableResult
  class func localPlayer() -> GKLocalPlayer
  var isAuthenticated: Bool { get }
  var isUnderage: Bool { get }
  @available(tvOS 6.0, *)
  var authenticateHandler: ((UIViewController?, NSError?) -> Void)?
  @available(tvOS 8.0, *)
  func loadFriendPlayers(completionHandler completionHandler: (([GKPlayer]?, NSError?) -> Void)? = nil)
  @available(tvOS 7.0, *)
  func setDefaultLeaderboardIdentifier(_ leaderboardIdentifier: String, completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  @available(tvOS 7.0, *)
  func loadDefaultLeaderboardIdentifier(completionHandler completionHandler: ((String?, NSError?) -> Void)? = nil)
  @available(tvOS 7.0, *)
  func generateIdentityVerificationSignature(completionHandler completionHandler: ((NSURL?, NSData?, NSData?, UInt64, NSError?) -> Void)? = nil)
}
protocol GKLocalPlayerListener : GKChallengeListener, GKInviteEventListener, GKTurnBasedEventListener, GKSavedGameListener {
}
extension GKLocalPlayer {
  @available(tvOS 7.0, *)
  func register(_ listener: GKLocalPlayerListener)
  @available(tvOS 7.0, *)
  func unregisterListener(_ listener: GKLocalPlayerListener)
  @available(tvOS 7.0, *)
  func unregisterAllListeners()
}
@available(tvOS 4.1, *)
let GKPlayerAuthenticationDidChangeNotificationName: String
extension GKLocalPlayer {
}
