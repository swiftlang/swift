
@available(OSX 10.8, *)
class GKNotificationBanner : NSObject {
  @available(OSX 10.8, *)
  class func show(withTitle title: String?, message message: String?, completionHandler completionHandler: (() -> Void)? = nil)
  @available(OSX 10.8, *)
  class func show(withTitle title: String?, message message: String?, duration duration: NSTimeInterval, completionHandler completionHandler: (() -> Void)? = nil)
}
