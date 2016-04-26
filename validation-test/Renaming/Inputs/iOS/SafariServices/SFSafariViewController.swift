
@available(iOS 9.0, *)
class SFSafariViewController : UIViewController {
  weak var delegate: @sil_weak SFSafariViewControllerDelegate?
  init(url URL: NSURL, entersReaderIfAvailable entersReaderIfAvailable: Bool)
  convenience init(url URL: NSURL)
}
@available(iOS 9.0, *)
protocol SFSafariViewControllerDelegate : NSObjectProtocol {
  @discardableResult
  optional func safariViewController(_ controller: SFSafariViewController, activityItemsFor URL: NSURL, title title: String?) -> [UIActivity]
  optional func safariViewControllerDidFinish(_ controller: SFSafariViewController)
  optional func safariViewController(_ controller: SFSafariViewController, didCompleteInitialLoad didLoadSuccessfully: Bool)
}
