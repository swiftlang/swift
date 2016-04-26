
class WebBackForwardList : NSObject {
  func add(_ item: WebHistoryItem!)
  func goBack()
  func goForward()
  func go(to item: WebHistoryItem!)
  var backItem: WebHistoryItem! { get }
  var currentItem: WebHistoryItem! { get }
  var forwardItem: WebHistoryItem! { get }
  @discardableResult
  func back(withLimit limit: Int32) -> [AnyObject]!
  @discardableResult
  func forwardList(withLimit limit: Int32) -> [AnyObject]!
  var capacity: Int32
  var backListCount: Int32 { get }
  var forwardListCount: Int32 { get }
  @discardableResult
  func contains(_ item: WebHistoryItem!) -> Bool
  @discardableResult
  func item(at index: Int32) -> WebHistoryItem!
}
extension WebBackForwardList {
  func setPageCacheSize(_ size: Int)
  @discardableResult
  func pageCacheSize() -> Int
}
