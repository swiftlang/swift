
@available(tvOS 7.0, *)
let UITransitionContextFromViewControllerKey: String
@available(tvOS 7.0, *)
let UITransitionContextToViewControllerKey: String
@available(tvOS 8.0, *)
let UITransitionContextFromViewKey: String
@available(tvOS 8.0, *)
let UITransitionContextToViewKey: String
protocol UIViewControllerContextTransitioning : NSObjectProtocol {
  @available(tvOS 2.0, *)
  @discardableResult
  func containerView() -> UIView?
  @discardableResult
  func isAnimated() -> Bool
  @discardableResult
  func isInteractive() -> Bool
  @discardableResult
  func transitionWasCancelled() -> Bool
  @discardableResult
  func presentationStyle() -> UIModalPresentationStyle
  func updateInteractiveTransition(_ percentComplete: CGFloat)
  func finishInteractiveTransition()
  func cancelInteractiveTransition()
  func completeTransition(_ didComplete: Bool)
  @available(tvOS 2.0, *)
  @discardableResult
  func viewController(forKey key: String) -> UIViewController?
  @available(tvOS 8.0, *)
  @discardableResult
  func view(forKey key: String) -> UIView?
  @available(tvOS 8.0, *)
  @discardableResult
  func targetTransform() -> CGAffineTransform
  @available(tvOS 2.0, *)
  @discardableResult
  func initialFrame(for vc: UIViewController) -> CGRect
  @available(tvOS 2.0, *)
  @discardableResult
  func finalFrame(for vc: UIViewController) -> CGRect
}
protocol UIViewControllerAnimatedTransitioning : NSObjectProtocol {
  @discardableResult
  func transitionDuration(_ transitionContext: UIViewControllerContextTransitioning?) -> NSTimeInterval
  func animateTransition(_ transitionContext: UIViewControllerContextTransitioning)
  optional func animationEnded(_ transitionCompleted: Bool)
}
protocol UIViewControllerInteractiveTransitioning : NSObjectProtocol {
  func startInteractiveTransition(_ transitionContext: UIViewControllerContextTransitioning)
  @discardableResult
  optional func completionSpeed() -> CGFloat
  @discardableResult
  optional func completionCurve() -> UIViewAnimationCurve
}
protocol UIViewControllerTransitioningDelegate : NSObjectProtocol {
  @available(tvOS 2.0, *)
  @discardableResult
  optional func animationController(forPresentedController presented: UIViewController, presenting presenting: UIViewController, sourceController source: UIViewController) -> UIViewControllerAnimatedTransitioning?
  @available(tvOS 2.0, *)
  @discardableResult
  optional func animationController(forDismissedController dismissed: UIViewController) -> UIViewControllerAnimatedTransitioning?
  @discardableResult
  optional func interactionController(forPresentation animator: UIViewControllerAnimatedTransitioning) -> UIViewControllerInteractiveTransitioning?
  @discardableResult
  optional func interactionController(forDismissal animator: UIViewControllerAnimatedTransitioning) -> UIViewControllerInteractiveTransitioning?
  @available(tvOS 8.0, *)
  @discardableResult
  optional func presentationController(forPresentedViewController presented: UIViewController, presenting presenting: UIViewController, sourceViewController source: UIViewController) -> UIPresentationController?
}
@available(tvOS 7.0, *)
class UIPercentDrivenInteractiveTransition : NSObject, UIViewControllerInteractiveTransitioning {
  var duration: CGFloat { get }
  var percentComplete: CGFloat { get }
  func update(_ percentComplete: CGFloat)
  func cancel()
  func finish()
}
