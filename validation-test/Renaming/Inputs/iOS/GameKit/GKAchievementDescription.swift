
@available(iOS 4.1, *)
class GKAchievementDescription : NSObject, NSCoding, NSSecureCoding {
  class func loadAchievementDescriptions(completionHandler completionHandler: (([GKAchievementDescription]?, NSError?) -> Void)? = nil)
  var identifier: String? { get }
  @available(iOS 6.0, *)
  var groupIdentifier: String? { get }
  var title: String? { get }
  var achievedDescription: String? { get }
  var unachievedDescription: String? { get }
  var maximumPoints: Int { get }
  var isHidden: Bool { get }
  @available(iOS 6.0, *)
  var isReplayable: Bool { get }
}
extension GKAchievementDescription {
  func loadImage(completionHandler completionHandler: ((UIImage?, NSError?) -> Void)? = nil)
  @discardableResult
  class func incompleteAchievementImage() -> UIImage
  @discardableResult
  class func placeholderCompletedAchievementImage() -> UIImage
}
