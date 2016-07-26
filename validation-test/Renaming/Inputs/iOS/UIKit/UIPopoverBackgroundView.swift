
protocol UIPopoverBackgroundViewMethods {
  @discardableResult
  static func arrowBase() -> CGFloat
  @discardableResult
  static func contentViewInsets() -> UIEdgeInsets
  @discardableResult
  static func arrowHeight() -> CGFloat
}
@available(iOS 5.0, *)
class UIPopoverBackgroundView : UIView, UIPopoverBackgroundViewMethods {
  var arrowOffset: CGFloat
  var arrowDirection: UIPopoverArrowDirection
  @available(iOS 6.0, *)
  @discardableResult
  class func wantsDefaultContentAppearance() -> Bool
}
