
@available(iOS 4.1, *)
class GKAchievement : NSObject, NSCoding, NSSecureCoding {
  class func loadAchievements(completionHandler completionHandler: (([GKAchievement]?, NSError?) -> Void)? = nil)
  class func resetAchievements(completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  init(identifier identifier: String?)
  @available(iOS 8.0, *)
  init(identifier identifier: String?, player player: GKPlayer)
  @available(iOS 6.0, *)
  class func report(_ achievements: [GKAchievement], withCompletionHandler completionHandler: ((NSError?) -> Void)? = nil)
  var identifier: String?
  var percentComplete: Double
  var isCompleted: Bool { get }
  @NSCopying var lastReportedDate: NSDate { get }
  @available(iOS 5.0, *)
  var showsCompletionBanner: Bool
  @available(iOS 8.0, *)
  var player: GKPlayer { get }
}
extension GKAchievement {
  @available(iOS, introduced: 7.0, deprecated: 8.0, message: "use initWithIdentifier:player:")
  init(identifier identifier: String?, forPlayer playerID: String)
  @available(iOS, introduced: 7.0, deprecated: 8.0, message: "use player")
  var playerID: String { get }
}
