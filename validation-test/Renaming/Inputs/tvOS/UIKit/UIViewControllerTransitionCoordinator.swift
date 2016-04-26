
protocol UIViewControllerTransitionCoordinatorContext : NSObjectProtocol {
  @discardableResult
  func isAnimated() -> Bool
  @discardableResult
  func presentationStyle() -> UIModalPresentationStyle
  @discardableResult
  func initiallyInteractive() -> Bool
  @discardableResult
  func isInteractive() -> Bool
  @discardableResult
  func isCancelled() -> Bool
  @discardableResult
  func transitionDuration() -> NSTimeInterval
  @discardableResult
  func percentComplete() -> CGFloat
  @discardableResult
  func completionVelocity() -> CGFloat
  @discardableResult
  func completionCurve() -> UIViewAnimationCurve
  @available(tvOS 2.0, *)
  @discardableResult
  func viewController(forKey key: String) -> UIViewController?
  @available(tvOS 8.0, *)
  @discardableResult
  func view(forKey key: String) -> UIView?
  @available(tvOS 2.0, *)
  @discardableResult
  func containerView() -> UIView
  @available(tvOS 8.0, *)
  @discardableResult
  func targetTransform() -> CGAffineTransform
}
protocol UIViewControllerTransitionCoordinator : UIViewControllerTransitionCoordinatorContext {
  @discardableResult
  func animate(alongsideTransition animation: ((UIViewControllerTransitionCoordinatorContext) -> Void)?, completion completion: ((UIViewControllerTransitionCoordinatorContext) -> Void)? = nil) -> Bool
  @available(tvOS 2.0, *)
  @discardableResult
  func animateAlongsideTransition(in view: UIView?, animation animation: ((UIViewControllerTransitionCoordinatorContext) -> Void)?, completion completion: ((UIViewControllerTransitionCoordinatorContext) -> Void)? = nil) -> Bool
  func notifyWhenInteractionEnds(_ handler: (UIViewControllerTransitionCoordinatorContext) -> Void)
}
extension UIViewController {
  @available(tvOS 7.0, *)
  @discardableResult
  func transitionCoordinator() -> UIViewControllerTransitionCoordinator?
}
