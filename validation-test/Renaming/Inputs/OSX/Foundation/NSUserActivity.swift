
@available(OSX 10.10, *)
class NSUserActivity : NSObject {
  init(activityType activityType: String)
  var activityType: String { get }
  var title: String?
  var userInfo: [NSObject : AnyObject]?
  func addUserInfoEntries(from otherDictionary: [NSObject : AnyObject])
  @available(OSX 10.11, *)
  var requiredUserInfoKeys: Set<String>
  var needsSave: Bool
  @NSCopying var webpageURL: NSURL?
  @available(OSX 10.11, *)
  @NSCopying var expirationDate: NSDate
  @available(OSX 10.11, *)
  var keywords: Set<String>
  var supportsContinuationStreams: Bool
  weak var delegate: @sil_weak NSUserActivityDelegate?
  func becomeCurrent()
  @available(OSX 10.11, *)
  func resignCurrent()
  func invalidate()
  func getContinuationStreams(completionHandler completionHandler: (NSInputStream?, NSOutputStream?, NSError?) -> Void)
  @available(OSX 10.11, *)
  var isEligibleForHandoff: Bool
  @available(OSX 10.11, *)
  var isEligibleForSearch: Bool
  @available(OSX 10.11, *)
  var isEligibleForPublicIndexing: Bool
}
@available(OSX 10.10, *)
let NSUserActivityTypeBrowsingWeb: String
@available(OSX 10.10, *)
protocol NSUserActivityDelegate : NSObjectProtocol {
  optional func userActivityWillSave(_ userActivity: NSUserActivity)
  optional func userActivityWasContinued(_ userActivity: NSUserActivity)
  optional func userActivity(_ userActivity: NSUserActivity?, didReceive inputStream: NSInputStream, outputStream outputStream: NSOutputStream)
}
