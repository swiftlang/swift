
@available(iOS 8.0, *)
class WKBackForwardList : NSObject {
  var currentItem: WKBackForwardListItem? { get }
  var backItem: WKBackForwardListItem? { get }
  var forwardItem: WKBackForwardListItem? { get }
  @discardableResult
  func item(at index: Int) -> WKBackForwardListItem?
  var backList: [WKBackForwardListItem] { get }
  var forwardList: [WKBackForwardListItem] { get }
}
