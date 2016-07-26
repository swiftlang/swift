
@available(OSX 10.8, *)
class GKLocalPlayer : GKPlayer {
  @discardableResult
  class func localPlayer() -> GKLocalPlayer
  var isAuthenticated: Bool { get }
  var isUnderage: Bool { get }
  @available(OSX 10.9, *)
  var authenticateHandler: ((NSViewController?, NSError?) -> Void)?
  @available(OSX 10.10, *)
  func loadFriendPlayers(completionHandler completionHandler: (([GKPlayer]?, NSError?) -> Void)? = nil)
  @available(OSX 10.10, *)
  func setDefaultLeaderboardIdentifier(_ leaderboardIdentifier: String, completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  @available(OSX 10.10, *)
  func loadDefaultLeaderboardIdentifier(completionHandler completionHandler: ((String?, NSError?) -> Void)? = nil)
  @available(OSX 10.10, *)
  func generateIdentityVerificationSignature(completionHandler completionHandler: ((NSURL?, NSData?, NSData?, UInt64, NSError?) -> Void)? = nil)
}
protocol GKLocalPlayerListener : GKChallengeListener, GKInviteEventListener, GKTurnBasedEventListener, GKSavedGameListener {
}
extension GKLocalPlayer {
  @available(OSX 10.10, *)
  func register(_ listener: GKLocalPlayerListener)
  @available(OSX 10.10, *)
  func unregisterListener(_ listener: GKLocalPlayerListener)
  @available(OSX 10.10, *)
  func unregisterAllListeners()
}
@available(OSX 10.8, *)
let GKPlayerAuthenticationDidChangeNotificationName: String
extension GKLocalPlayer {
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "Use setDefaultLeaderboardIdentifier:completionHandler: instead")
  func setDefaultLeaderboardCategoryID(_ categoryID: String?, completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "Use loadDefaultLeaderboardIdentifierWithCompletionHandler: instead")
  func loadDefaultLeaderboardCategoryID(completionHandler completionHandler: ((String?, NSError?) -> Void)? = nil)
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "use loadFriendPlayersWithCompletionHandler: instead")
  func loadFriends(completionHandler completionHandler: (([String]?, NSError?) -> Void)? = nil)
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "use loadFriendPlayersWithCompletionHandler: instead")
  var friends: [String]? { get }
}
