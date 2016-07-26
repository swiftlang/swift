
class NSTimer : NSObject {
  /*not inherited*/ init(timeInterval ti: NSTimeInterval, invocation invocation: NSInvocation, repeats yesOrNo: Bool)
  @discardableResult
  class func scheduledTimer(timeInterval ti: NSTimeInterval, invocation invocation: NSInvocation, repeats yesOrNo: Bool) -> NSTimer
  /*not inherited*/ init(timeInterval ti: NSTimeInterval, target aTarget: AnyObject, selector aSelector: Selector, userInfo userInfo: AnyObject?, repeats yesOrNo: Bool)
  @discardableResult
  class func scheduledTimer(timeInterval ti: NSTimeInterval, target aTarget: AnyObject, selector aSelector: Selector, userInfo userInfo: AnyObject?, repeats yesOrNo: Bool) -> NSTimer
  init(fireAt date: NSDate, interval ti: NSTimeInterval, target t: AnyObject, selector s: Selector, userInfo ui: AnyObject?, repeats rep: Bool)
  func fire()
  @NSCopying var fireDate: NSDate
  var timeInterval: NSTimeInterval { get }
  @available(watchOS 2.0, *)
  var tolerance: NSTimeInterval
  func invalidate()
  var isValid: Bool { get }
  var userInfo: AnyObject? { get }
}
