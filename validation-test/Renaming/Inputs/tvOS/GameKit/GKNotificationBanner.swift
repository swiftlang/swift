
@available(tvOS 5.0, *)
class GKNotificationBanner : NSObject {
  @available(tvOS 5.0, *)
  class func show(withTitle title: String?, message message: String?, completionHandler completionHandler: (() -> Void)? = nil)
  @available(tvOS 6.0, *)
  class func show(withTitle title: String?, message message: String?, duration duration: NSTimeInterval, completionHandler completionHandler: (() -> Void)? = nil)
}
