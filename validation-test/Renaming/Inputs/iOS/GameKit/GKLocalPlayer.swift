
@available(iOS 4.1, *)
class GKLocalPlayer : GKPlayer {
  @discardableResult
  class func localPlayer() -> GKLocalPlayer
  var isAuthenticated: Bool { get }
  var isUnderage: Bool { get }
  @available(iOS 6.0, *)
  var authenticateHandler: ((UIViewController?, NSError?) -> Void)?
  @available(iOS 8.0, *)
  func loadFriendPlayers(completionHandler completionHandler: (([GKPlayer]?, NSError?) -> Void)? = nil)
  @available(iOS 7.0, *)
  func setDefaultLeaderboardIdentifier(_ leaderboardIdentifier: String, completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  @available(iOS 7.0, *)
  func loadDefaultLeaderboardIdentifier(completionHandler completionHandler: ((String?, NSError?) -> Void)? = nil)
  @available(iOS 7.0, *)
  func generateIdentityVerificationSignature(completionHandler completionHandler: ((NSURL?, NSData?, NSData?, UInt64, NSError?) -> Void)? = nil)
}
protocol GKLocalPlayerListener : GKChallengeListener, GKInviteEventListener, GKTurnBasedEventListener, GKSavedGameListener {
}
extension GKLocalPlayer {
  @available(iOS 7.0, *)
  func register(_ listener: GKLocalPlayerListener)
  @available(iOS 7.0, *)
  func unregisterListener(_ listener: GKLocalPlayerListener)
  @available(iOS 7.0, *)
  func unregisterAllListeners()
}
@available(iOS 4.1, *)
let GKPlayerAuthenticationDidChangeNotificationName: String
extension GKLocalPlayer {
  @available(iOS, introduced: 4.1, deprecated: 8.0, message: "use loadFriendPlayersWithCompletionHandler: instead")
  func loadFriends(completionHandler completionHandler: (([String]?, NSError?) -> Void)? = nil)
  @available(iOS, introduced: 4.1, deprecated: 8.0, message: "use loadFriendPlayersWithCompletionHandler: instead")
  var friends: [String]? { get }
}
