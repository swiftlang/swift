
typealias SCNAnimationEventBlock = (CAAnimation, AnyObject, Bool) -> Void
@available(tvOS 8.0, *)
class SCNAnimationEvent : NSObject {
  convenience init(keyTime time: CGFloat, block eventBlock: SCNAnimationEventBlock)
}
protocol SCNAnimatable : NSObjectProtocol {
  func add(_ animation: CAAnimation, forKey key: String?)
  func removeAllAnimations()
  func removeAnimation(forKey key: String)
  var animationKeys: [String] { get }
  @discardableResult
  func animation(forKey key: String) -> CAAnimation?
  @available(tvOS 8.0, *)
  func pauseAnimation(forKey key: String)
  @available(tvOS 8.0, *)
  func resumeAnimation(forKey key: String)
  @available(tvOS 8.0, *)
  @discardableResult
  func isAnimation(forKeyPaused key: String) -> Bool
  @available(tvOS 8.0, *)
  func removeAnimation(forKey key: String, fadeOutDuration duration: CGFloat)
}
extension CAAnimation {
  var usesSceneTimeBase: Bool
  @available(tvOS 8.0, *)
  var fadeInDuration: CGFloat
  @available(tvOS 8.0, *)
  var fadeOutDuration: CGFloat
  @available(tvOS 8.0, *)
  var animationEvents: [SCNAnimationEvent]?
}
