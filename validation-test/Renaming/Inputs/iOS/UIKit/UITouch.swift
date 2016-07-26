
enum UITouchPhase : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case began
  case moved
  case stationary
  case ended
  case cancelled
}
enum UIForceTouchCapability : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case unavailable
  case available
}
@available(iOS 9.0, *)
enum UITouchType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case direct
  case indirect
  @available(iOS 9.1, *)
  case stylus
}
@available(iOS 9.1, *)
struct UITouchProperties : OptionSet {
  init(rawValue rawValue: Int)
  let rawValue: Int
  static var force: UITouchProperties { get }
  static var azimuth: UITouchProperties { get }
  static var altitude: UITouchProperties { get }
  static var location: UITouchProperties { get }
}
@available(iOS 2.0, *)
class UITouch : NSObject {
  var timestamp: NSTimeInterval { get }
  var phase: UITouchPhase { get }
  var tapCount: Int { get }
  @available(iOS 9.0, *)
  var type: UITouchType { get }
  @available(iOS 8.0, *)
  var majorRadius: CGFloat { get }
  @available(iOS 8.0, *)
  var majorRadiusTolerance: CGFloat { get }
  var window: UIWindow? { get }
  var view: UIView? { get }
  @available(iOS 3.2, *)
  var gestureRecognizers: [UIGestureRecognizer]? { get }
  @discardableResult
  func location(in view: UIView?) -> CGPoint
  @discardableResult
  func previousLocation(in view: UIView?) -> CGPoint
  @available(iOS 9.1, *)
  @discardableResult
  func preciseLocation(in view: UIView?) -> CGPoint
  @available(iOS 9.1, *)
  @discardableResult
  func precisePreviousLocation(in view: UIView?) -> CGPoint
  @available(iOS 9.0, *)
  var force: CGFloat { get }
  @available(iOS 9.0, *)
  var maximumPossibleForce: CGFloat { get }
  @available(iOS 9.1, *)
  @discardableResult
  func azimuthAngle(in view: UIView?) -> CGFloat
  @available(iOS 9.1, *)
  @discardableResult
  func azimuthUnitVector(in view: UIView?) -> CGVector
  @available(iOS 9.1, *)
  var altitudeAngle: CGFloat { get }
  @available(iOS 9.1, *)
  var estimationUpdateIndex: NSNumber? { get }
  @available(iOS 9.1, *)
  var estimatedProperties: UITouchProperties { get }
  @available(iOS 9.1, *)
  var estimatedPropertiesExpectingUpdates: UITouchProperties { get }
}
