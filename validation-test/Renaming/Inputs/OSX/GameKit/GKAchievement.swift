
@available(OSX 10.8, *)
class GKAchievement : NSObject, NSCoding, NSSecureCoding {
  class func loadAchievements(completionHandler completionHandler: (([GKAchievement]?, NSError?) -> Void)? = nil)
  class func resetAchievements(completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  init(identifier identifier: String?)
  @available(OSX 10.10, *)
  init(identifier identifier: String?, player player: GKPlayer)
  @available(OSX 10.8, *)
  class func report(_ achievements: [GKAchievement], withCompletionHandler completionHandler: ((NSError?) -> Void)? = nil)
  var identifier: String?
  var percentComplete: Double
  var isCompleted: Bool { get }
  @NSCopying var lastReportedDate: NSDate { get }
  @available(OSX 10.8, *)
  var showsCompletionBanner: Bool
  @available(OSX 10.10, *)
  var player: GKPlayer { get }
}
extension GKAchievement {
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "Use +reportAchievements:withCompletionHandler:")
  func report(completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "Use isHidden on the GKAchievementDescription class instead")
  var isHidden: Bool { get }
}
