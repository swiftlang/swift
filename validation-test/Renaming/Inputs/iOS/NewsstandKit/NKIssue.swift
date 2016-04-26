
@available(iOS 5.0, *)
let NKIssueDownloadCompletedNotification: String
@available(iOS 5.0, *)
enum NKIssueContentStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case downloading
  case available
}
@available(iOS 5.0, *)
class NKIssue : NSObject {
  var downloadingAssets: [NKAssetDownload] { get }
  @NSCopying var contentURL: NSURL { get }
  var status: NKIssueContentStatus { get }
  var name: String { get }
  @NSCopying var date: NSDate { get }
  @discardableResult
  func addAsset(with request: NSURLRequest) -> NKAssetDownload
}
