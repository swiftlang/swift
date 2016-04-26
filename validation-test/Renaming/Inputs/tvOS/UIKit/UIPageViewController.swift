
enum UIPageViewControllerNavigationOrientation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case horizontal
  case vertical
}
enum UIPageViewControllerSpineLocation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case min
  case mid
  case max
}
enum UIPageViewControllerNavigationDirection : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case forward
  case reverse
}
enum UIPageViewControllerTransitionStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case pageCurl
  case scroll
}
let UIPageViewControllerOptionSpineLocationKey: String
@available(tvOS 6.0, *)
let UIPageViewControllerOptionInterPageSpacingKey: String
@available(tvOS 5.0, *)
class UIPageViewController : UIViewController {
  init(transitionStyle style: UIPageViewControllerTransitionStyle, navigationOrientation navigationOrientation: UIPageViewControllerNavigationOrientation, options options: [String : AnyObject]? = [:])
  weak var delegate: @sil_weak UIPageViewControllerDelegate?
  weak var dataSource: @sil_weak UIPageViewControllerDataSource?
  var transitionStyle: UIPageViewControllerTransitionStyle { get }
  var navigationOrientation: UIPageViewControllerNavigationOrientation { get }
  var spineLocation: UIPageViewControllerSpineLocation { get }
  var isDoubleSided: Bool
  var gestureRecognizers: [UIGestureRecognizer] { get }
  var viewControllers: [UIViewController]? { get }
  func setViewControllers(_ viewControllers: [UIViewController]?, direction direction: UIPageViewControllerNavigationDirection, animated animated: Bool, completion completion: ((Bool) -> Void)? = nil)
}
protocol UIPageViewControllerDelegate : NSObjectProtocol {
  @available(tvOS 6.0, *)
  optional func pageViewController(_ pageViewController: UIPageViewController, willTransitionTo pendingViewControllers: [UIViewController])
  @available(tvOS 5.0, *)
  optional func pageViewController(_ pageViewController: UIPageViewController, didFinishAnimating finished: Bool, previousViewControllers previousViewControllers: [UIViewController], transitionCompleted completed: Bool)
}
protocol UIPageViewControllerDataSource : NSObjectProtocol {
  @available(tvOS 5.0, *)
  @discardableResult
  func pageViewController(_ pageViewController: UIPageViewController, viewControllerBefore viewController: UIViewController) -> UIViewController?
  @available(tvOS 5.0, *)
  @discardableResult
  func pageViewController(_ pageViewController: UIPageViewController, viewControllerAfter viewController: UIViewController) -> UIViewController?
  @available(tvOS 6.0, *)
  @discardableResult
  optional func presentationCount(for pageViewController: UIPageViewController) -> Int
  @available(tvOS 6.0, *)
  @discardableResult
  optional func presentationIndex(for pageViewController: UIPageViewController) -> Int
}
