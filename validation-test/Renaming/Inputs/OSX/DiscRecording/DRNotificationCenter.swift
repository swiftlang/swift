
class DRNotificationCenter : NSObject {
  @discardableResult
  class func currentRunLoop() -> DRNotificationCenter!
  func addObserver(_ observer: AnyObject!, selector aSelector: Selector!, name notificationName: String!, object anObject: AnyObject!)
  func removeObserver(_ observer: AnyObject!, name aName: String!, object anObject: AnyObject!)
}
