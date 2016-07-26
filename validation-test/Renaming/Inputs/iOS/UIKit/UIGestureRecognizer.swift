
enum UIGestureRecognizerState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case possible
  case began
  case changed
  case ended
  case cancelled
  case failed
  static var recognized: UIGestureRecognizerState { get }
}
@available(iOS 3.2, *)
class UIGestureRecognizer : NSObject {
  init(target target: AnyObject?, action action: Selector?)
  func addTarget(_ target: AnyObject, action action: Selector)
  func removeTarget(_ target: AnyObject?, action action: Selector?)
  weak var delegate: @sil_weak UIGestureRecognizerDelegate?
  var isEnabled: Bool
  var view: UIView? { get }
  var cancelsTouchesInView: Bool
  var delaysTouchesBegan: Bool
  var delaysTouchesEnded: Bool
  @available(iOS 9.0, *)
  var allowedTouchTypes: [NSNumber]
  @available(iOS 9.0, *)
  var allowedPressTypes: [NSNumber]
  func require(toFail otherGestureRecognizer: UIGestureRecognizer)
  @discardableResult
  func location(in view: UIView?) -> CGPoint
  @discardableResult
  func numberOfTouches() -> Int
  @discardableResult
  func location(ofTouch touchIndex: Int, in view: UIView?) -> CGPoint
}
protocol UIGestureRecognizerDelegate : NSObjectProtocol {
  @available(iOS 3.2, *)
  @discardableResult
  optional func gestureRecognizerShouldBegin(_ gestureRecognizer: UIGestureRecognizer) -> Bool
  @available(iOS 3.2, *)
  @discardableResult
  optional func gestureRecognizer(_ gestureRecognizer: UIGestureRecognizer, shouldRecognizeSimultaneouslyWith otherGestureRecognizer: UIGestureRecognizer) -> Bool
  @available(iOS 7.0, *)
  @discardableResult
  optional func gestureRecognizer(_ gestureRecognizer: UIGestureRecognizer, shouldRequireFailureOf otherGestureRecognizer: UIGestureRecognizer) -> Bool
  @available(iOS 7.0, *)
  @discardableResult
  optional func gestureRecognizer(_ gestureRecognizer: UIGestureRecognizer, shouldBeRequiredToFailBy otherGestureRecognizer: UIGestureRecognizer) -> Bool
  @available(iOS 3.2, *)
  @discardableResult
  optional func gestureRecognizer(_ gestureRecognizer: UIGestureRecognizer, shouldReceive touch: UITouch) -> Bool
  @available(iOS 9.0, *)
  @discardableResult
  optional func gestureRecognizer(_ gestureRecognizer: UIGestureRecognizer, shouldReceive press: UIPress) -> Bool
}
