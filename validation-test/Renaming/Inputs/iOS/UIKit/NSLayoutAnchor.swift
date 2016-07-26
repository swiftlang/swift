
@available(iOS 9.0, *)
class NSLayoutAnchor<AnchorType : AnyObject> : NSObject {
  @discardableResult
  func constraintEqual(to anchor: NSLayoutAnchor<AnchorType>!) -> NSLayoutConstraint!
  @discardableResult
  func constraintGreaterThanOrEqual(to anchor: NSLayoutAnchor<AnchorType>!) -> NSLayoutConstraint!
  @discardableResult
  func constraintLessThanOrEqual(to anchor: NSLayoutAnchor<AnchorType>!) -> NSLayoutConstraint!
  @discardableResult
  func constraintEqual(to anchor: NSLayoutAnchor<AnchorType>!, constant c: CGFloat) -> NSLayoutConstraint!
  @discardableResult
  func constraintGreaterThanOrEqual(to anchor: NSLayoutAnchor<AnchorType>!, constant c: CGFloat) -> NSLayoutConstraint!
  @discardableResult
  func constraintLessThanOrEqual(to anchor: NSLayoutAnchor<AnchorType>!, constant c: CGFloat) -> NSLayoutConstraint!
  init()
}
@available(iOS 9.0, *)
class NSLayoutXAxisAnchor : NSLayoutAnchor<NSLayoutXAxisAnchor> {
}
@available(iOS 9.0, *)
class NSLayoutYAxisAnchor : NSLayoutAnchor<NSLayoutYAxisAnchor> {
}
@available(iOS 9.0, *)
class NSLayoutDimension : NSLayoutAnchor<NSLayoutDimension> {
  @discardableResult
  func constraintEqual(toConstant c: CGFloat) -> NSLayoutConstraint!
  @discardableResult
  func constraintGreaterThanOrEqual(toConstant c: CGFloat) -> NSLayoutConstraint!
  @discardableResult
  func constraintLessThanOrEqual(toConstant c: CGFloat) -> NSLayoutConstraint!
  @discardableResult
  func constraintEqual(toAnchor anchor: NSLayoutDimension!, multiplier m: CGFloat) -> NSLayoutConstraint!
  @discardableResult
  func constraintGreaterThanOrEqual(toAnchor anchor: NSLayoutDimension!, multiplier m: CGFloat) -> NSLayoutConstraint!
  @discardableResult
  func constraintLessThanOrEqual(toAnchor anchor: NSLayoutDimension!, multiplier m: CGFloat) -> NSLayoutConstraint!
  @discardableResult
  func constraintEqual(toAnchor anchor: NSLayoutDimension!, multiplier m: CGFloat, constant c: CGFloat) -> NSLayoutConstraint!
  @discardableResult
  func constraintGreaterThanOrEqual(toAnchor anchor: NSLayoutDimension!, multiplier m: CGFloat, constant c: CGFloat) -> NSLayoutConstraint!
  @discardableResult
  func constraintLessThanOrEqual(toAnchor anchor: NSLayoutDimension!, multiplier m: CGFloat, constant c: CGFloat) -> NSLayoutConstraint!
}
