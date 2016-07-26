
typealias GKChallengeComposeCompletionBlock = (NSViewController, Bool, [String]?) -> Void
enum GKChallengeState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case invalid
  case pending
  case completed
  case declined
}
@available(OSX 10.8, *)
class GKChallenge : NSObject, NSCoding, NSSecureCoding {
  class func loadReceivedChallenges(completionHandler completionHandler: (([GKChallenge]?, NSError?) -> Void)? = nil)
  func decline()
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "Use issuingPlayer instead")
  var issuingPlayerID: String? { get }
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "Use receivingPlayer instead")
  var receivingPlayerID: String? { get }
  @available(OSX 10.10, *)
  @NSCopying var issuingPlayer: GKPlayer? { get }
  @available(OSX 10.10, *)
  @NSCopying var receivingPlayer: GKPlayer? { get }
  var state: GKChallengeState { get }
  var issueDate: NSDate { get }
  var completionDate: NSDate? { get }
  var message: String? { get }
}
@available(OSX 10.8, *)
class GKScoreChallenge : GKChallenge {
  var score: GKScore? { get }
}
@available(OSX 10.8, *)
class GKAchievementChallenge : GKChallenge {
  var achievement: GKAchievement? { get }
}
extension GKScore {
  @available(OSX 10.10, *)
  @discardableResult
  func challengeComposeController(withMessage message: String?, players players: [GKPlayer]?, completionHandler completionHandler: GKChallengeComposeCompletionBlock? = nil) -> NSViewController
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "pass GKPlayers to challengeComposeControllerWithMessage:players:completionHandler: and present the view controller instead")
  func issueChallenge(toPlayers playerIDs: [String]?, message message: String?)
  @available(OSX 10.10, *)
  class func report(_ scores: [GKScore], withEligibleChallenges challenges: [GKChallenge], withCompletionHandler completionHandler: ((NSError?) -> Void)? = nil)
}
extension GKAchievement {
  @available(OSX 10.10, *)
  @discardableResult
  func challengeComposeController(withMessage message: String?, players players: [GKPlayer], completionHandler completionHandler: GKChallengeComposeCompletionBlock? = nil) -> NSViewController
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "pass GKPlayers to challengeComposeControllerWithMessage:players:completionHandler: and present the view controller instead")
  func issueChallenge(toPlayers playerIDs: [String]?, message message: String?)
  @available(OSX 10.10, *)
  func selectChallengeablePlayers(_ players: [GKPlayer], withCompletionHandler completionHandler: (([GKPlayer]?, NSError?) -> Void)? = nil)
  @available(OSX 10.10, *)
  class func report(_ achievements: [GKAchievement], withEligibleChallenges challenges: [GKChallenge], withCompletionHandler completionHandler: ((NSError?) -> Void)? = nil)
}
extension GKScore {
}
extension GKAchievement {
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "pass GKPlayers to selectChallengeablePlayers:")
  func selectChallengeablePlayerIDs(_ playerIDs: [String]?, withCompletionHandler completionHandler: (([String]?, NSError?) -> Void)? = nil)
}
