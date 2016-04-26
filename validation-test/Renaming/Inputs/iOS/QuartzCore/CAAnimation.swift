
class CAAnimation : NSObject, NSCoding, NSCopying, CAMediaTiming, CAAction {
  @discardableResult
  class func defaultValue(forKey key: String) -> AnyObject?
  @discardableResult
  func shouldArchiveValue(forKey key: String) -> Bool
  var timingFunction: CAMediaTimingFunction?
  var delegate: AnyObject?
  var isRemovedOnCompletion: Bool
}
extension NSObject {
  class func animationDidStart(_ anim: CAAnimation)
  func animationDidStart(_ anim: CAAnimation)
  class func animationDidStop(_ anim: CAAnimation, finished flag: Bool)
  func animationDidStop(_ anim: CAAnimation, finished flag: Bool)
}
class CAPropertyAnimation : CAAnimation {
  convenience init(keyPath path: String?)
  var keyPath: String?
  var isAdditive: Bool
  var isCumulative: Bool
  var valueFunction: CAValueFunction?
}
class CABasicAnimation : CAPropertyAnimation {
  var fromValue: AnyObject?
  var toValue: AnyObject?
  var byValue: AnyObject?
}
class CAKeyframeAnimation : CAPropertyAnimation {
  var values: [AnyObject]?
  var path: CGPath?
  var keyTimes: [NSNumber]?
  var timingFunctions: [CAMediaTimingFunction]?
  var calculationMode: String
  var tensionValues: [NSNumber]?
  var continuityValues: [NSNumber]?
  var biasValues: [NSNumber]?
  var rotationMode: String?
}
@available(iOS 2.0, *)
let kCAAnimationLinear: String
@available(iOS 2.0, *)
let kCAAnimationDiscrete: String
@available(iOS 2.0, *)
let kCAAnimationPaced: String
@available(iOS 4.0, *)
let kCAAnimationCubic: String
@available(iOS 4.0, *)
let kCAAnimationCubicPaced: String
@available(iOS 2.0, *)
let kCAAnimationRotateAuto: String
@available(iOS 2.0, *)
let kCAAnimationRotateAutoReverse: String
class CASpringAnimation : CABasicAnimation {
  var mass: CGFloat
  var stiffness: CGFloat
  var damping: CGFloat
  var initialVelocity: CGFloat
  var settlingDuration: CFTimeInterval { get }
}
class CATransition : CAAnimation {
  var type: String
  var subtype: String?
  var startProgress: Float
  var endProgress: Float
  var filter: AnyObject?
}
@available(iOS 2.0, *)
let kCATransitionFade: String
@available(iOS 2.0, *)
let kCATransitionMoveIn: String
@available(iOS 2.0, *)
let kCATransitionPush: String
@available(iOS 2.0, *)
let kCATransitionReveal: String
@available(iOS 2.0, *)
let kCATransitionFromRight: String
@available(iOS 2.0, *)
let kCATransitionFromLeft: String
@available(iOS 2.0, *)
let kCATransitionFromTop: String
@available(iOS 2.0, *)
let kCATransitionFromBottom: String
class CAAnimationGroup : CAAnimation {
  var animations: [CAAnimation]?
}
