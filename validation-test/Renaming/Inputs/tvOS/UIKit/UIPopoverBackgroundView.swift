
protocol UIPopoverBackgroundViewMethods {
  @discardableResult
  static func arrowBase() -> CGFloat
  @discardableResult
  static func contentViewInsets() -> UIEdgeInsets
  @discardableResult
  static func arrowHeight() -> CGFloat
}
@available(tvOS 5.0, *)
class UIPopoverBackgroundView : UIView, UIPopoverBackgroundViewMethods {
  var arrowOffset: CGFloat
  var arrowDirection: UIPopoverArrowDirection
  @available(tvOS 6.0, *)
  @discardableResult
  class func wantsDefaultContentAppearance() -> Bool
}
