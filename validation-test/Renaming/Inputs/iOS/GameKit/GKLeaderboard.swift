
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
@available(iOS 4.1, *)
class GKLeaderboard : NSObject {
  var timeScope: GKLeaderboardTimeScope
  var playerScope: GKLeaderboardPlayerScope
  @available(iOS 7.0, *)
  var identifier: String?
  var title: String? { get }
  var range: NSRange
  var scores: [GKScore]? { get }
  var maxRange: Int { get }
  var localPlayerScore: GKScore? { get }
  var isLoading: Bool { get }
  @available(iOS 6.0, *)
  var groupIdentifier: String? { get }
  @available(iOS 8.0, *)
  init(players players: [GKPlayer])
  func loadScores(completionHandler completionHandler: (([GKScore]?, NSError?) -> Void)? = nil)
  @available(iOS 6.0, *)
  class func loadLeaderboards(completionHandler completionHandler: (([GKLeaderboard]?, NSError?) -> Void)? = nil)
}
extension GKLeaderboard {
  @available(iOS, introduced: 4.1, deprecated: 8.0, message: "Use initWithPlayers: instead")
  init?(playerIDs playerIDs: [String]?)
}
extension GKLeaderboard {
  @available(iOS 7.0, *)
  func loadImage(completionHandler completionHandler: ((UIImage?, NSError?) -> Void)? = nil)
}
