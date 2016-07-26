
typealias UIActivityViewControllerCompletionHandler = (String?, Bool) -> Void
typealias UIActivityViewControllerCompletionWithItemsHandler = (String?, Bool, [AnyObject]?, NSError?) -> Void
@available(iOS 6.0, *)
class UIActivityViewController : UIViewController {
  init(activityItems activityItems: [AnyObject], applicationActivities applicationActivities: [UIActivity]?)
  @available(iOS, introduced: 6.0, deprecated: 8.0, message: "Use completionWithItemsHandler instead.")
  var completionHandler: UIActivityViewControllerCompletionHandler?
  @available(iOS 8.0, *)
  var completionWithItemsHandler: UIActivityViewControllerCompletionWithItemsHandler?
  var excludedActivityTypes: [String]?
}
