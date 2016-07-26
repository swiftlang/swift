
enum GKLeaderboardTimeScope : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case today
  case week
  case allTime
}
enum GKLeaderboardPlayerScope : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case global
  case friendsOnly
}
@available(OSX 10.8, *)
class GKLeaderboard : NSObject {
  var timeScope: GKLeaderboardTimeScope
  var playerScope: GKLeaderboardPlayerScope
  @available(OSX 10.10, *)
  var identifier: String?
  var title: String? { get }
  var range: NSRange
  var scores: [GKScore]? { get }
  var maxRange: Int { get }
  var localPlayerScore: GKScore? { get }
  var isLoading: Bool { get }
  @available(OSX 10.9, *)
  var groupIdentifier: String? { get }
  @available(OSX 10.10, *)
  init(players players: [GKPlayer])
  func loadScores(completionHandler completionHandler: (([GKScore]?, NSError?) -> Void)? = nil)
  @available(OSX 10.8, *)
  class func loadLeaderboards(completionHandler completionHandler: (([GKLeaderboard]?, NSError?) -> Void)? = nil)
}
extension GKLeaderboard {
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "Use identifier instead")
  var category: String?
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "Use initWithPlayers: instead")
  init?(playerIDs playerIDs: [String]?)
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "Use setDefaultLeaderboardIdentifier:completionHandler: on GKLocalPlayer instead")
  class func setDefaultLeaderboard(_ leaderboardIdentifier: String?, withCompletionHandler completionHandler: ((NSError?) -> Void)? = nil)
}
extension GKLeaderboard {
  @available(OSX 10.8, *)
  func loadImage(completionHandler completionHandler: ((NSImage?, NSError?) -> Void)? = nil)
}
