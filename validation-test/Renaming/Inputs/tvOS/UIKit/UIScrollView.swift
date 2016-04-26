
enum UIScrollViewIndicatorStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case black
  case white
}
@available(tvOS 7.0, *)
enum UIScrollViewKeyboardDismissMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case onDrag
  case interactive
}
@available(tvOS 3.0, *)
let UIScrollViewDecelerationRateNormal: CGFloat
@available(tvOS 3.0, *)
let UIScrollViewDecelerationRateFast: CGFloat
@available(tvOS 2.0, *)
class UIScrollView : UIView, NSCoding {
  var contentOffset: CGPoint
  var contentSize: CGSize
  var contentInset: UIEdgeInsets
  weak var delegate: @sil_weak UIScrollViewDelegate?
  var isDirectionalLockEnabled: Bool
  var bounces: Bool
  var alwaysBounceVertical: Bool
  var alwaysBounceHorizontal: Bool
  var isScrollEnabled: Bool
  var showsHorizontalScrollIndicator: Bool
  var showsVerticalScrollIndicator: Bool
  var scrollIndicatorInsets: UIEdgeInsets
  var indicatorStyle: UIScrollViewIndicatorStyle
  @available(tvOS 3.0, *)
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
  @available(tvOS 3.0, *)
  var zoomScale: CGFloat
  @available(tvOS 3.0, *)
  func setZoomScale(_ scale: CGFloat, animated animated: Bool)
  @available(tvOS 3.0, *)
  func zoom(to rect: CGRect, animated animated: Bool)
  var bouncesZoom: Bool
  var isZooming: Bool { get }
  var isZoomBouncing: Bool { get }
  @available(tvOS 5.0, *)
  var panGestureRecognizer: UIPanGestureRecognizer { get }
  @available(tvOS 5.0, *)
  var pinchGestureRecognizer: UIPinchGestureRecognizer? { get }
  @available(tvOS 9.0, *)
  var directionalPressGestureRecognizer: UIGestureRecognizer { get }
  @available(tvOS 7.0, *)
  var keyboardDismissMode: UIScrollViewKeyboardDismissMode
}
protocol UIScrollViewDelegate : NSObjectProtocol {
  @available(tvOS 2.0, *)
  optional func scrollViewDidScroll(_ scrollView: UIScrollView)
  @available(tvOS 3.2, *)
  optional func scrollViewDidZoom(_ scrollView: UIScrollView)
  @available(tvOS 2.0, *)
  optional func scrollViewWillBeginDragging(_ scrollView: UIScrollView)
  @available(tvOS 5.0, *)
  optional func scrollViewWillEndDragging(_ scrollView: UIScrollView, withVelocity velocity: CGPoint, targetContentOffset targetContentOffset: UnsafeMutablePointer<CGPoint>)
  @available(tvOS 2.0, *)
  optional func scrollViewDidEndDragging(_ scrollView: UIScrollView, willDecelerate decelerate: Bool)
  @available(tvOS 2.0, *)
  optional func scrollViewWillBeginDecelerating(_ scrollView: UIScrollView)
  @available(tvOS 2.0, *)
  optional func scrollViewDidEndDecelerating(_ scrollView: UIScrollView)
  @available(tvOS 2.0, *)
  optional func scrollViewDidEndScrollingAnimation(_ scrollView: UIScrollView)
  @available(tvOS 2.0, *)
  @discardableResult
  optional func viewForZooming(in scrollView: UIScrollView) -> UIView?
  @available(tvOS 3.2, *)
  optional func scrollViewWillBeginZooming(_ scrollView: UIScrollView, with view: UIView?)
  @available(tvOS 2.0, *)
  optional func scrollViewDidEndZooming(_ scrollView: UIScrollView, with view: UIView?, atScale scale: CGFloat)
  @available(tvOS 2.0, *)
  @discardableResult
  optional func scrollViewShouldScroll(toTop scrollView: UIScrollView) -> Bool
  @available(tvOS 2.0, *)
  optional func scrollViewDidScroll(toTop scrollView: UIScrollView)
}
