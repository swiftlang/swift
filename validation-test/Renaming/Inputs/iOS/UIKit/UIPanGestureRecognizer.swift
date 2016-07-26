
@available(iOS 3.2, *)
class UIPanGestureRecognizer : UIGestureRecognizer {
  var minimumNumberOfTouches: Int
  var maximumNumberOfTouches: Int
  @discardableResult
  func translation(in view: UIView?) -> CGPoint
  func setTranslation(_ translation: CGPoint, in view: UIView?)
  @discardableResult
  func velocity(in view: UIView?) -> CGPoint
}
