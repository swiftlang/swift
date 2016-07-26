
@available(iOS 6.0, *)
class UIRefreshControl : UIControl {
  var isRefreshing: Bool { get }
  var attributedTitle: NSAttributedString?
  @available(iOS 6.0, *)
  func beginRefreshing()
  @available(iOS 6.0, *)
  func endRefreshing()
}
