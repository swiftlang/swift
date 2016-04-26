
@available(tvOS 3.2, *)
class UIPanGestureRecognizer : UIGestureRecognizer {
  @discardableResult
  func translation(in view: UIView?) -> CGPoint
  func setTranslation(_ translation: CGPoint, in view: UIView?)
  @discardableResult
  func velocity(in view: UIView?) -> CGPoint
}
