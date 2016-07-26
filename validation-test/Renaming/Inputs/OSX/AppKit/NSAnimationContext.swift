
@available(OSX 10.5, *)
class NSAnimationContext : NSObject {
  @available(OSX 10.7, *)
  class func runAnimationGroup(_ changes: (NSAnimationContext) -> Void, completionHandler completionHandler: (() -> Void)? = nil)
  class func beginGrouping()
  class func endGrouping()
  @discardableResult
  class func current() -> NSAnimationContext
  var duration: NSTimeInterval
  @available(OSX 10.7, *)
  var timingFunction: CAMediaTimingFunction?
  @available(OSX 10.7, *)
  var completionHandler: (() -> Void)?
  @available(OSX 10.8, *)
  var allowsImplicitAnimation: Bool
}
