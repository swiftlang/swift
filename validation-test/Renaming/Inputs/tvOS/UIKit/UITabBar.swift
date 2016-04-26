
@available(tvOS 7.0, *)
enum UITabBarItemPositioning : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case automatic
  case fill
  case centered
}
@available(tvOS 2.0, *)
class UITabBar : UIView {
  unowned(unsafe) var delegate: @sil_unmanaged UITabBarDelegate?
  var items: [UITabBarItem]?
  unowned(unsafe) var selectedItem: @sil_unmanaged UITabBarItem?
  func setItems(_ items: [UITabBarItem]?, animated animated: Bool)
  @available(tvOS 7.0, *)
  var barTintColor: UIColor?
  @available(tvOS 5.0, *)
  var backgroundImage: UIImage?
  @available(tvOS 5.0, *)
  var selectionIndicatorImage: UIImage?
  @available(tvOS 6.0, *)
  var shadowImage: UIImage?
  @available(tvOS 7.0, *)
  var itemWidth: CGFloat
  @available(tvOS 7.0, *)
  var itemSpacing: CGFloat
  @available(tvOS 7.0, *)
  var isTranslucent: Bool
}
protocol UITabBarDelegate : NSObjectProtocol {
  @available(tvOS 2.0, *)
  optional func tabBar(_ tabBar: UITabBar, didSelect item: UITabBarItem)
}
