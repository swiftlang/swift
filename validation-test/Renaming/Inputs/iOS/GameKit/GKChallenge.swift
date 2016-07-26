
typealias GKChallengeComposeCompletionBlock = (UIViewController, Bool, [String]?) -> Void
enum GKChallengeState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case invalid
  case pending
  case completed
  case declined
}
@available(iOS 6.0, *)
class GKChallenge : NSObject, NSCoding, NSSecureCoding {
  class func loadReceivedChallenges(completionHandler completionHandler: (([GKChallenge]?, NSError?) -> Void)? = nil)
  func decline()
  @available(iOS, introduced: 6.0, deprecated: 8.0, message: "Use issuingPlayer instead")
  var issuingPlayerID: String? { get }
  @available(iOS, introduced: 6.0, deprecated: 8.0, message: "Use receivingPlayer instead")
  var receivingPlayerID: String? { get }
  @available(iOS 8.0, *)
  @NSCopying var issuingPlayer: GKPlayer? { get }
  @available(iOS 8.0, *)
  @NSCopying var receivingPlayer: GKPlayer? { get }
  var state: GKChallengeState { get }
  var issueDate: NSDate { get }
  var completionDate: NSDate? { get }
  var message: String? { get }
}
@available(iOS 6.0, *)
class GKScoreChallenge : GKChallenge {
  var score: GKScore? { get }
}
@available(iOS 6.0, *)
class GKAchievementChallenge : GKChallenge {
  var achievement: GKAchievement? { get }
}
extension GKScore {
  @available(iOS 8.0, *)
  @discardableResult
  func challengeComposeController(withMessage message: String?, players players: [GKPlayer]?, completionHandler completionHandler: GKChallengeComposeCompletionBlock? = nil) -> UIViewController
  @available(iOS 7.0, *)
  class func report(_ scores: [GKScore], withEligibleChallenges challenges: [GKChallenge], withCompletionHandler completionHandler: ((NSError?) -> Void)? = nil)
}
extension GKAchievement {
  @available(iOS 8.0, *)
  @discardableResult
  func challengeComposeController(withMessage message: String?, players players: [GKPlayer], completionHandler completionHandler: GKChallengeComposeCompletionBlock? = nil) -> UIViewController
  @available(iOS 8.0, *)
  func selectChallengeablePlayers(_ players: [GKPlayer], withCompletionHandler completionHandler: (([GKPlayer]?, NSError?) -> Void)? = nil)
  @available(iOS 7.0, *)
  class func report(_ achievements: [GKAchievement], withEligibleChallenges challenges: [GKChallenge], withCompletionHandler completionHandler: ((NSError?) -> Void)? = nil)
}
extension GKScore {
  @available(iOS, introduced: 7.0, deprecated: 8.0, message: "pass GKPlayers to challengeComposeControllerWithMessage:players:")
  @discardableResult
  func challengeComposeController(withPlayers playerIDs: [String]?, message message: String?, completionHandler completionHandler: GKChallengeComposeCompletionBlock? = nil) -> UIViewController?
}
extension GKAchievement {
  @available(iOS, introduced: 6.0, deprecated: 8.0, message: "pass GKPlayers to selectChallengeablePlayers:")
  func selectChallengeablePlayerIDs(_ playerIDs: [String]?, withCompletionHandler completionHandler: (([String]?, NSError?) -> Void)? = nil)
  @available(iOS, introduced: 7.0, deprecated: 8.0, message: "pass GKPlayers to challengeComposeControllerWithMessage:players:")
  @discardableResult
  func challengeComposeController(withPlayers playerIDs: [String]?, message message: String?, completionHandler completionHandler: GKChallengeComposeCompletionBlock? = nil) -> UIViewController?
}
