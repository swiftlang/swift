
@available(iOS 5.0, *)
class NKLibrary : NSObject {
  var issues: [NKIssue] { get }
  var downloadingAssets: [NKAssetDownload] { get }
  var currentlyReadingIssue: NKIssue?
  @discardableResult
  class func shared() -> NKLibrary?
  @discardableResult
  func issue(withName name: String) -> NKIssue?
  @discardableResult
  func addIssue(withName name: String, date date: NSDate) -> NKIssue
  func removeIssue(_ issue: NKIssue)
}
