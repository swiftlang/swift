
class CATransaction : NSObject {
  class func begin()
  class func commit()
  class func flush()
  class func lock()
  class func unlock()
  @discardableResult
  class func animationDuration() -> CFTimeInterval
  class func setAnimationDuration(_ dur: CFTimeInterval)
  @discardableResult
  class func animationTimingFunction() -> CAMediaTimingFunction?
  class func setAnimationTimingFunction(_ function: CAMediaTimingFunction?)
  @discardableResult
  class func disableActions() -> Bool
  class func setDisableActions(_ flag: Bool)
  @discardableResult
  class func completionBlock() -> (() -> Void)?
  class func setCompletionBlock(_ block: (() -> Void)?)
}
@available(tvOS 2.0, *)
let kCATransactionAnimationDuration: String
@available(tvOS 2.0, *)
let kCATransactionDisableActions: String
@available(tvOS 3.0, *)
let kCATransactionAnimationTimingFunction: String
@available(tvOS 4.0, *)
let kCATransactionCompletionBlock: String
