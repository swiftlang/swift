
typealias SCNAnimationEventBlock = (CAAnimation, AnyObject, Bool) -> Void
@available(OSX 10.9, *)
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
  @available(OSX 10.9, *)
  func pauseAnimation(forKey key: String)
  @available(OSX 10.9, *)
  func resumeAnimation(forKey key: String)
  @available(OSX 10.9, *)
  @discardableResult
  func isAnimation(forKeyPaused key: String) -> Bool
  @available(OSX 10.10, *)
  func removeAnimation(forKey key: String, fadeOutDuration duration: CGFloat)
}
extension CAAnimation {
  var usesSceneTimeBase: Bool
  @available(OSX 10.9, *)
  var fadeInDuration: CGFloat
  @available(OSX 10.9, *)
  var fadeOutDuration: CGFloat
  @available(OSX 10.9, *)
  var animationEvents: [SCNAnimationEvent]?
}
