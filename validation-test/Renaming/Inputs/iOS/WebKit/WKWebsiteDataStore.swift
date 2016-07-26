
@available(iOS 9.0, *)
class WKWebsiteDataStore : NSObject {
  @discardableResult
  class func defaultDataStore() -> WKWebsiteDataStore
  @discardableResult
  class func nonPersistent() -> WKWebsiteDataStore
  var isPersistent: Bool { get }
  @discardableResult
  class func allWebsiteDataTypes() -> Set<String>
  func fetchDataRecords(ofTypes dataTypes: Set<String>, completionHandler completionHandler: ([WKWebsiteDataRecord]) -> Void)
  func removeData(ofTypes dataTypes: Set<String>, for dataRecords: [WKWebsiteDataRecord], completionHandler completionHandler: () -> Void)
  func removeData(ofTypes websiteDataTypes: Set<String>, modifiedSince date: NSDate, completionHandler completionHandler: () -> Void)
}
