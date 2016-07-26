
@available(iOS 4.1, *)
class GKScore : NSObject, NSCoding, NSSecureCoding {
  init(leaderboardIdentifier identifier: String)
  @available(iOS 8.0, *)
  init(leaderboardIdentifier identifier: String, player player: GKPlayer)
  var value: Int64
  var formattedValue: String? { get }
  @available(iOS 7.0, *)
  var leaderboardIdentifier: String
  @available(iOS 5.0, *)
  var context: UInt64
  var date: NSDate { get }
  @available(iOS 8.0, *)
  var player: GKPlayer { get }
  var rank: Int { get }
  @available(iOS 5.0, *)
  var shouldSetDefaultLeaderboard: Bool
  @available(iOS 6.0, *)
  class func report(_ scores: [GKScore], withCompletionHandler completionHandler: ((NSError?) -> Void)? = nil)
}
extension GKScore {
  @available(iOS, introduced: 7.0, deprecated: 8.0, message: "use initWithLeaderboardIdentifier:player:")
  init(leaderboardIdentifier identifier: String, forPlayer playerID: String)
  @available(iOS, introduced: 4.1, deprecated: 8.0, message: "use player")
  var playerID: String { get }
}
