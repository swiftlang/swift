
@available(tvOS 8.0, *)
class SCNTransaction : NSObject {
  class func begin()
  class func commit()
  class func flush()
  class func lock()
  class func unlock()
  @discardableResult
  class func animationDuration() -> CFTimeInterval
  class func setAnimationDuration(_ duration: CFTimeInterval)
  @discardableResult
  class func animationTimingFunction() -> CAMediaTimingFunction?
  class func setAnimationTimingFunction(_ animationTimingFunction: CAMediaTimingFunction?)
  @discardableResult
  class func disableActions() -> Bool
  class func setDisableActions(_ flag: Bool)
  @discardableResult
  class func completionBlock() -> (() -> Void)?
  class func setCompletionBlock(_ block: (() -> Void)?)
}
