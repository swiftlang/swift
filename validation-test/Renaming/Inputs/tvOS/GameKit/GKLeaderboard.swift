
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
@available(tvOS 4.1, *)
class GKLeaderboard : NSObject {
  var timeScope: GKLeaderboardTimeScope
  var playerScope: GKLeaderboardPlayerScope
  @available(tvOS 7.0, *)
  var identifier: String?
  var title: String? { get }
  var range: NSRange
  var scores: [GKScore]? { get }
  var maxRange: Int { get }
  var localPlayerScore: GKScore? { get }
  var isLoading: Bool { get }
  @available(tvOS 6.0, *)
  var groupIdentifier: String? { get }
  @available(tvOS 8.0, *)
  init(players players: [GKPlayer])
  func loadScores(completionHandler completionHandler: (([GKScore]?, NSError?) -> Void)? = nil)
  @available(tvOS 6.0, *)
  class func loadLeaderboards(completionHandler completionHandler: (([GKLeaderboard]?, NSError?) -> Void)? = nil)
}
extension GKLeaderboard {
}
extension GKLeaderboard {
}
