
@available(tvOS 2.0, *)
class UINavigationBar : UIView, NSCoding, UIBarPositioning {
  weak var delegate: @sil_weak UINavigationBarDelegate?
  @available(tvOS 3.0, *)
  var isTranslucent: Bool
  func push(_ item: UINavigationItem, animated animated: Bool)
  @discardableResult
  func popNavigationItem(animated animated: Bool) -> UINavigationItem?
  var topItem: UINavigationItem? { get }
  var backItem: UINavigationItem? { get }
  var items: [UINavigationItem]?
  func setItems(_ items: [UINavigationItem]?, animated animated: Bool)
  @available(tvOS 7.0, *)
  var barTintColor: UIColor?
  @available(tvOS 7.0, *)
  func setBackgroundImage(_ backgroundImage: UIImage?, for barPosition: UIBarPosition, barMetrics barMetrics: UIBarMetrics)
  @available(tvOS 7.0, *)
  @discardableResult
  func backgroundImage(for barPosition: UIBarPosition, barMetrics barMetrics: UIBarMetrics) -> UIImage?
  @available(tvOS 5.0, *)
  func setBackgroundImage(_ backgroundImage: UIImage?, for barMetrics: UIBarMetrics)
  @available(tvOS 5.0, *)
  @discardableResult
  func backgroundImage(for barMetrics: UIBarMetrics) -> UIImage?
  @available(tvOS 6.0, *)
  var shadowImage: UIImage?
  @available(tvOS 5.0, *)
  var titleTextAttributes: [String : AnyObject]?
  @available(tvOS 5.0, *)
  func setTitleVerticalPositionAdjustment(_ adjustment: CGFloat, for barMetrics: UIBarMetrics)
  @available(tvOS 5.0, *)
  @discardableResult
  func titleVerticalPositionAdjustment(for barMetrics: UIBarMetrics) -> CGFloat
}
protocol UINavigationBarDelegate : UIBarPositioningDelegate {
  @available(tvOS 2.0, *)
  @discardableResult
  optional func navigationBar(_ navigationBar: UINavigationBar, shouldPush item: UINavigationItem) -> Bool
  @available(tvOS 2.0, *)
  optional func navigationBar(_ navigationBar: UINavigationBar, didPush item: UINavigationItem)
  @available(tvOS 2.0, *)
  @discardableResult
  optional func navigationBar(_ navigationBar: UINavigationBar, shouldPop item: UINavigationItem) -> Bool
  @available(tvOS 2.0, *)
  optional func navigationBar(_ navigationBar: UINavigationBar, didPop item: UINavigationItem)
}
@available(tvOS 2.0, *)
class UINavigationItem : NSObject, NSCoding {
  init(title title: String)
  var title: String?
  var titleView: UIView?
  @available(tvOS 5.0, *)
  var leftBarButtonItems: [UIBarButtonItem]?
  @available(tvOS 5.0, *)
  var rightBarButtonItems: [UIBarButtonItem]?
  @available(tvOS 5.0, *)
  func setLeftBarButtonItems(_ items: [UIBarButtonItem]?, animated animated: Bool)
  @available(tvOS 5.0, *)
  func setRightBarButtonItems(_ items: [UIBarButtonItem]?, animated animated: Bool)
  var leftBarButtonItem: UIBarButtonItem?
  var rightBarButtonItem: UIBarButtonItem?
  func setLeftBarButtonItem(_ item: UIBarButtonItem?, animated animated: Bool)
  func setRightBarButtonItem(_ item: UIBarButtonItem?, animated animated: Bool)
}
