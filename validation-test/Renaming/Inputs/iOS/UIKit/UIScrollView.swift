
enum UIScrollViewIndicatorStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case black
  case white
}
@available(iOS 7.0, *)
enum UIScrollViewKeyboardDismissMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case onDrag
  case interactive
}
@available(iOS 3.0, *)
let UIScrollViewDecelerationRateNormal: CGFloat
@available(iOS 3.0, *)
let UIScrollViewDecelerationRateFast: CGFloat
@available(iOS 2.0, *)
class UIScrollView : UIView, NSCoding {
  var contentOffset: CGPoint
  var contentSize: CGSize
  var contentInset: UIEdgeInsets
  weak var delegate: @sil_weak UIScrollViewDelegate?
  var isDirectionalLockEnabled: Bool
  var bounces: Bool
  var alwaysBounceVertical: Bool
  var alwaysBounceHorizontal: Bool
  var isPagingEnabled: Bool
  var isScrollEnabled: Bool
  var showsHorizontalScrollIndicator: Bool
  var showsVerticalScrollIndicator: Bool
  var scrollIndicatorInsets: UIEdgeInsets
  var indicatorStyle: UIScrollViewIndicatorStyle
  @available(iOS 3.0, *)
  var decelerationRate: CGFloat
  func setContentOffset(_ contentOffset: CGPoint, animated animated: Bool)
  func scrollRectToVisible(_ rect: CGRect, animated animated: Bool)
  func flashScrollIndicators()
  var isTracking: Bool { get }
  var isDragging: Bool { get }
  var isDecelerating: Bool { get }
  var delaysContentTouches: Bool
  var canCancelContentTouches: Bool
  @discardableResult
  func touchesShouldBegin(_ touches: Set<UITouch>, with event: UIEvent?, inContentView view: UIView) -> Bool
  @discardableResult
  func touchesShouldCancel(inContentView view: UIView) -> Bool
  var minimumZoomScale: CGFloat
  var maximumZoomScale: CGFloat
  @available(iOS 3.0, *)
  var zoomScale: CGFloat
  @available(iOS 3.0, *)
  func setZoomScale(_ scale: CGFloat, animated animated: Bool)
  @available(iOS 3.0, *)
  func zoom(to rect: CGRect, animated animated: Bool)
  var bouncesZoom: Bool
  var isZooming: Bool { get }
  var isZoomBouncing: Bool { get }
  var scrollsToTop: Bool
  @available(iOS 5.0, *)
  var panGestureRecognizer: UIPanGestureRecognizer { get }
  @available(iOS 5.0, *)
  var pinchGestureRecognizer: UIPinchGestureRecognizer? { get }
  @available(iOS 7.0, *)
  var keyboardDismissMode: UIScrollViewKeyboardDismissMode
}
protocol UIScrollViewDelegate : NSObjectProtocol {
  @available(iOS 2.0, *)
  optional func scrollViewDidScroll(_ scrollView: UIScrollView)
  @available(iOS 3.2, *)
  optional func scrollViewDidZoom(_ scrollView: UIScrollView)
  @available(iOS 2.0, *)
  optional func scrollViewWillBeginDragging(_ scrollView: UIScrollView)
  @available(iOS 5.0, *)
  optional func scrollViewWillEndDragging(_ scrollView: UIScrollView, withVelocity velocity: CGPoint, targetContentOffset targetContentOffset: UnsafeMutablePointer<CGPoint>)
  @available(iOS 2.0, *)
  optional func scrollViewDidEndDragging(_ scrollView: UIScrollView, willDecelerate decelerate: Bool)
  @available(iOS 2.0, *)
  optional func scrollViewWillBeginDecelerating(_ scrollView: UIScrollView)
  @available(iOS 2.0, *)
  optional func scrollViewDidEndDecelerating(_ scrollView: UIScrollView)
  @available(iOS 2.0, *)
  optional func scrollViewDidEndScrollingAnimation(_ scrollView: UIScrollView)
  @available(iOS 2.0, *)
  @discardableResult
  optional func viewForZooming(in scrollView: UIScrollView) -> UIView?
  @available(iOS 3.2, *)
  optional func scrollViewWillBeginZooming(_ scrollView: UIScrollView, with view: UIView?)
  @available(iOS 2.0, *)
  optional func scrollViewDidEndZooming(_ scrollView: UIScrollView, with view: UIView?, atScale scale: CGFloat)
  @available(iOS 2.0, *)
  @discardableResult
  optional func scrollViewShouldScroll(toTop scrollView: UIScrollView) -> Bool
  @available(iOS 2.0, *)
  optional func scrollViewDidScroll(toTop scrollView: UIScrollView)
}
