
enum UIEventType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case touches
  case motion
  case remoteControl
  @available(iOS 9.0, *)
  case presses
}
enum UIEventSubtype : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case motionShake
  case remoteControlPlay
  case remoteControlPause
  case remoteControlStop
  case remoteControlTogglePlayPause
  case remoteControlNextTrack
  case remoteControlPreviousTrack
  case remoteControlBeginSeekingBackward
  case remoteControlEndSeekingBackward
  case remoteControlBeginSeekingForward
  case remoteControlEndSeekingForward
}
@available(iOS 2.0, *)
class UIEvent : NSObject {
  @available(iOS 3.0, *)
  var type: UIEventType { get }
  @available(iOS 3.0, *)
  var subtype: UIEventSubtype { get }
  var timestamp: NSTimeInterval { get }
  @discardableResult
  func allTouches() -> Set<UITouch>?
  @discardableResult
  func touches(for window: UIWindow) -> Set<UITouch>?
  @discardableResult
  func touches(for view: UIView) -> Set<UITouch>?
  @available(iOS 3.2, *)
  @discardableResult
  func touches(for gesture: UIGestureRecognizer) -> Set<UITouch>?
  @available(iOS 9.0, *)
  @discardableResult
  func coalescedTouches(for touch: UITouch) -> [UITouch]?
  @available(iOS 9.0, *)
  @discardableResult
  func predictedTouches(for touch: UITouch) -> [UITouch]?
}
