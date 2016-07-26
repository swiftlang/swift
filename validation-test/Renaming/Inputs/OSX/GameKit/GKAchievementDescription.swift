
@available(OSX 10.8, *)
class GKAchievementDescription : NSObject, NSCoding, NSSecureCoding {
  class func loadAchievementDescriptions(completionHandler completionHandler: (([GKAchievementDescription]?, NSError?) -> Void)? = nil)
  var identifier: String? { get }
  @available(OSX 10.8, *)
  var groupIdentifier: String? { get }
  var title: String? { get }
  var achievedDescription: String? { get }
  var unachievedDescription: String? { get }
  var maximumPoints: Int { get }
  var isHidden: Bool { get }
  @available(OSX 10.8, *)
  var isReplayable: Bool { get }
}
extension GKAchievementDescription {
  @available(OSX 10.8, *)
  var image: NSImage? { get }
  func loadImage(completionHandler completionHandler: ((NSImage?, NSError?) -> Void)? = nil)
  @discardableResult
  class func incompleteAchievementImage() -> NSImage
  @discardableResult
  class func placeholderCompletedAchievementImage() -> NSImage
}
