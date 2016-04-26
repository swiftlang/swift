
@available(iOS 9.0, *)
enum UIPressPhase : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case began
  case changed
  case stationary
  case ended
  case cancelled
}
@available(iOS 9.0, *)
enum UIPressType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case upArrow
  case downArrow
  case leftArrow
  case rightArrow
  case select
  case menu
  case playPause
}
@available(iOS 9.0, *)
class UIPress : NSObject {
  var timestamp: NSTimeInterval { get }
  var phase: UIPressPhase { get }
  var type: UIPressType { get }
  var window: UIWindow? { get }
  var responder: UIResponder? { get }
  var gestureRecognizers: [UIGestureRecognizer]? { get }
  var force: CGFloat { get }
}
