
@available(tvOS 8.0, *)
class NSUserActivity : NSObject {
  init(activityType activityType: String)
  var activityType: String { get }
  var title: String?
  var userInfo: [NSObject : AnyObject]?
  func addUserInfoEntries(from otherDictionary: [NSObject : AnyObject])
  @available(tvOS 9.0, *)
  var requiredUserInfoKeys: Set<String>
  var needsSave: Bool
  @NSCopying var webpageURL: NSURL?
  @available(tvOS 9.0, *)
  @NSCopying var expirationDate: NSDate
  @available(tvOS 9.0, *)
  var keywords: Set<String>
  var supportsContinuationStreams: Bool
  weak var delegate: @sil_weak NSUserActivityDelegate?
  func becomeCurrent()
  @available(tvOS 9.0, *)
  func resignCurrent()
  func invalidate()
  func getContinuationStreams(completionHandler completionHandler: (NSInputStream?, NSOutputStream?, NSError?) -> Void)
  @available(tvOS 9.0, *)
  var isEligibleForHandoff: Bool
  @available(tvOS 9.0, *)
  var isEligibleForSearch: Bool
  @available(tvOS 9.0, *)
  var isEligibleForPublicIndexing: Bool
}
@available(tvOS 8.0, *)
let NSUserActivityTypeBrowsingWeb: String
@available(tvOS 8.0, *)
protocol NSUserActivityDelegate : NSObjectProtocol {
  optional func userActivityWillSave(_ userActivity: NSUserActivity)
  optional func userActivityWasContinued(_ userActivity: NSUserActivity)
  optional func userActivity(_ userActivity: NSUserActivity?, didReceive inputStream: NSInputStream, outputStream outputStream: NSOutputStream)
}
