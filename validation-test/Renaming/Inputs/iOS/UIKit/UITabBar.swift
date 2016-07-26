
@available(iOS 7.0, *)
enum UITabBarItemPositioning : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case automatic
  case fill
  case centered
}
@available(iOS 2.0, *)
class UITabBar : UIView {
  unowned(unsafe) var delegate: @sil_unmanaged UITabBarDelegate?
  var items: [UITabBarItem]?
  unowned(unsafe) var selectedItem: @sil_unmanaged UITabBarItem?
  func setItems(_ items: [UITabBarItem]?, animated animated: Bool)
  func beginCustomizingItems(_ items: [UITabBarItem])
  @discardableResult
  func endCustomizing(animated animated: Bool) -> Bool
  @discardableResult
  func isCustomizing() -> Bool
  @available(iOS 7.0, *)
  var barTintColor: UIColor?
  @available(iOS, introduced: 5.0, deprecated: 8.0, message: "Use tintColor")
  var selectedImageTintColor: UIColor?
  @available(iOS 5.0, *)
  var backgroundImage: UIImage?
  @available(iOS 5.0, *)
  var selectionIndicatorImage: UIImage?
  @available(iOS 6.0, *)
  var shadowImage: UIImage?
  @available(iOS 7.0, *)
  var itemPositioning: UITabBarItemPositioning
  @available(iOS 7.0, *)
  var itemWidth: CGFloat
  @available(iOS 7.0, *)
  var itemSpacing: CGFloat
  @available(iOS 7.0, *)
  var barStyle: UIBarStyle
  @available(iOS 7.0, *)
  var isTranslucent: Bool
}
protocol UITabBarDelegate : NSObjectProtocol {
  @available(iOS 2.0, *)
  optional func tabBar(_ tabBar: UITabBar, didSelect item: UITabBarItem)
  @available(iOS 2.0, *)
  optional func tabBar(_ tabBar: UITabBar, willBeginCustomizing items: [UITabBarItem])
  @available(iOS 2.0, *)
  optional func tabBar(_ tabBar: UITabBar, didBeginCustomizing items: [UITabBarItem])
  @available(iOS 2.0, *)
  optional func tabBar(_ tabBar: UITabBar, willEndCustomizing items: [UITabBarItem], changed changed: Bool)
  @available(iOS 2.0, *)
  optional func tabBar(_ tabBar: UITabBar, didEndCustomizing items: [UITabBarItem], changed changed: Bool)
}
