
@available(iOS 2.0, *)
class UINavigationBar : UIView, NSCoding, UIBarPositioning {
  var barStyle: UIBarStyle
  weak var delegate: @sil_weak UINavigationBarDelegate?
  @available(iOS 3.0, *)
  var isTranslucent: Bool
  func push(_ item: UINavigationItem, animated animated: Bool)
  @discardableResult
  func popNavigationItem(animated animated: Bool) -> UINavigationItem?
  var topItem: UINavigationItem? { get }
  var backItem: UINavigationItem? { get }
  var items: [UINavigationItem]?
  func setItems(_ items: [UINavigationItem]?, animated animated: Bool)
  @available(iOS 7.0, *)
  var barTintColor: UIColor?
  @available(iOS 7.0, *)
  func setBackgroundImage(_ backgroundImage: UIImage?, for barPosition: UIBarPosition, barMetrics barMetrics: UIBarMetrics)
  @available(iOS 7.0, *)
  @discardableResult
  func backgroundImage(for barPosition: UIBarPosition, barMetrics barMetrics: UIBarMetrics) -> UIImage?
  @available(iOS 5.0, *)
  func setBackgroundImage(_ backgroundImage: UIImage?, for barMetrics: UIBarMetrics)
  @available(iOS 5.0, *)
  @discardableResult
  func backgroundImage(for barMetrics: UIBarMetrics) -> UIImage?
  @available(iOS 6.0, *)
  var shadowImage: UIImage?
  @available(iOS 5.0, *)
  var titleTextAttributes: [String : AnyObject]?
  @available(iOS 5.0, *)
  func setTitleVerticalPositionAdjustment(_ adjustment: CGFloat, for barMetrics: UIBarMetrics)
  @available(iOS 5.0, *)
  @discardableResult
  func titleVerticalPositionAdjustment(for barMetrics: UIBarMetrics) -> CGFloat
  @available(iOS 7.0, *)
  var backIndicatorImage: UIImage?
  @available(iOS 7.0, *)
  var backIndicatorTransitionMaskImage: UIImage?
}
protocol UINavigationBarDelegate : UIBarPositioningDelegate {
  @available(iOS 2.0, *)
  @discardableResult
  optional func navigationBar(_ navigationBar: UINavigationBar, shouldPush item: UINavigationItem) -> Bool
  @available(iOS 2.0, *)
  optional func navigationBar(_ navigationBar: UINavigationBar, didPush item: UINavigationItem)
  @available(iOS 2.0, *)
  @discardableResult
  optional func navigationBar(_ navigationBar: UINavigationBar, shouldPop item: UINavigationItem) -> Bool
  @available(iOS 2.0, *)
  optional func navigationBar(_ navigationBar: UINavigationBar, didPop item: UINavigationItem)
}
@available(iOS 2.0, *)
class UINavigationItem : NSObject, NSCoding {
  init(title title: String)
  var title: String?
  var titleView: UIView?
  var prompt: String?
  var backBarButtonItem: UIBarButtonItem?
  var hidesBackButton: Bool
  func setHidesBackButton(_ hidesBackButton: Bool, animated animated: Bool)
  @available(iOS 5.0, *)
  var leftBarButtonItems: [UIBarButtonItem]?
  @available(iOS 5.0, *)
  var rightBarButtonItems: [UIBarButtonItem]?
  @available(iOS 5.0, *)
  func setLeftBarButtonItems(_ items: [UIBarButtonItem]?, animated animated: Bool)
  @available(iOS 5.0, *)
  func setRightBarButtonItems(_ items: [UIBarButtonItem]?, animated animated: Bool)
  @available(iOS 5.0, *)
  var leftItemsSupplementBackButton: Bool
  var leftBarButtonItem: UIBarButtonItem?
  var rightBarButtonItem: UIBarButtonItem?
  func setLeftBarButtonItem(_ item: UIBarButtonItem?, animated animated: Bool)
  func setRightBarButtonItem(_ item: UIBarButtonItem?, animated animated: Bool)
}
