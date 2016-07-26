
typealias GKChallengeComposeCompletionBlock = (UIViewController, Bool, [String]?) -> Void
enum GKChallengeState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case invalid
  case pending
  case completed
  case declined
}
@available(tvOS 6.0, *)
class GKChallenge : NSObject, NSCoding, NSSecureCoding {
  class func loadReceivedChallenges(completionHandler completionHandler: (([GKChallenge]?, NSError?) -> Void)? = nil)
  func decline()
  @available(tvOS 8.0, *)
  @NSCopying var issuingPlayer: GKPlayer? { get }
  @available(tvOS 8.0, *)
  @NSCopying var receivingPlayer: GKPlayer? { get }
  var state: GKChallengeState { get }
  var issueDate: NSDate { get }
  var completionDate: NSDate? { get }
  var message: String? { get }
}
@available(tvOS 6.0, *)
class GKScoreChallenge : GKChallenge {
  var score: GKScore? { get }
}
@available(tvOS 6.0, *)
class GKAchievementChallenge : GKChallenge {
  var achievement: GKAchievement? { get }
}
extension GKScore {
  @available(tvOS 8.0, *)
  @discardableResult
  func challengeComposeController(withMessage message: String?, players players: [GKPlayer]?, completionHandler completionHandler: GKChallengeComposeCompletionBlock? = nil) -> UIViewController
  @available(tvOS 7.0, *)
  class func report(_ scores: [GKScore], withEligibleChallenges challenges: [GKChallenge], withCompletionHandler completionHandler: ((NSError?) -> Void)? = nil)
}
extension GKAchievement {
  @available(tvOS 8.0, *)
  @discardableResult
  func challengeComposeController(withMessage message: String?, players players: [GKPlayer], completionHandler completionHandler: GKChallengeComposeCompletionBlock? = nil) -> UIViewController
  @available(tvOS 8.0, *)
  func selectChallengeablePlayers(_ players: [GKPlayer], withCompletionHandler completionHandler: (([GKPlayer]?, NSError?) -> Void)? = nil)
  @available(tvOS 7.0, *)
  class func report(_ achievements: [GKAchievement], withEligibleChallenges challenges: [GKChallenge], withCompletionHandler completionHandler: ((NSError?) -> Void)? = nil)
}
extension GKScore {
}
extension GKAchievement {
}
