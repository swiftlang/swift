
@available(iOS 2.0, *)
class UIToolbar : UIView, UIBarPositioning {
  var barStyle: UIBarStyle
  var items: [UIBarButtonItem]?
  @available(iOS 3.0, *)
  var isTranslucent: Bool
  func setItems(_ items: [UIBarButtonItem]?, animated animated: Bool)
  @available(iOS 7.0, *)
  var barTintColor: UIColor?
  @available(iOS 5.0, *)
  func setBackgroundImage(_ backgroundImage: UIImage?, forToolbarPosition topOrBottom: UIBarPosition, barMetrics barMetrics: UIBarMetrics)
  @available(iOS 5.0, *)
  @discardableResult
  func backgroundImage(forToolbarPosition topOrBottom: UIBarPosition, barMetrics barMetrics: UIBarMetrics) -> UIImage?
  @available(iOS 6.0, *)
  func setShadowImage(_ shadowImage: UIImage?, forToolbarPosition topOrBottom: UIBarPosition)
  @available(iOS 6.0, *)
  @discardableResult
  func shadowImage(forToolbarPosition topOrBottom: UIBarPosition) -> UIImage?
  @available(iOS 7.0, *)
  unowned(unsafe) var delegate: @sil_unmanaged UIToolbarDelegate?
}
protocol UIToolbarDelegate : UIBarPositioningDelegate {
}
