
@available(iOS 5.0, *)
class GKNotificationBanner : NSObject {
  @available(iOS 5.0, *)
  class func show(withTitle title: String?, message message: String?, completionHandler completionHandler: (() -> Void)? = nil)
  @available(iOS 6.0, *)
  class func show(withTitle title: String?, message message: String?, duration duration: NSTimeInterval, completionHandler completionHandler: (() -> Void)? = nil)
}
