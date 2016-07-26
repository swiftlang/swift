
@available(tvOS 8.0, *)
enum SCNActionTimingMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case linear
  case easeIn
  case easeOut
  case easeInEaseOut
}
typealias SCNActionTimingFunction = (Float) -> Float
protocol SCNActionable : NSObjectProtocol {
  @available(tvOS 8.0, *)
  func run(_ action: SCNAction)
  @available(tvOS 8.0, *)
  func run(_ action: SCNAction, completionHandler block: (() -> Void)? = nil)
  @available(tvOS 8.0, *)
  func run(_ action: SCNAction, forKey key: String?)
  @available(tvOS 8.0, *)
  func run(_ action: SCNAction, forKey key: String?, completionHandler block: (() -> Void)? = nil)
  @available(tvOS 8.0, *)
  var hasActions: Bool { get }
  @available(tvOS 8.0, *)
  @discardableResult
  func action(forKey key: String) -> SCNAction?
  @available(tvOS 8.0, *)
  func removeAction(forKey key: String)
  @available(tvOS 8.0, *)
  func removeAllActions()
  @available(tvOS 8.0, *)
  var actionKeys: [String] { get }
}
@available(tvOS 8.0, *)
class SCNAction : NSObject, NSCopying, NSSecureCoding {
  var duration: NSTimeInterval
  var timingMode: SCNActionTimingMode
  var timingFunction: SCNActionTimingFunction?
  var speed: CGFloat
  @discardableResult
  func reversed() -> SCNAction
  @discardableResult
  class func moveBy(x deltaX: CGFloat, y deltaY: CGFloat, z deltaZ: CGFloat, duration duration: NSTimeInterval) -> SCNAction
  @discardableResult
  class func move(by delta: SCNVector3, duration duration: NSTimeInterval) -> SCNAction
  @discardableResult
  class func move(to location: SCNVector3, duration duration: NSTimeInterval) -> SCNAction
  @discardableResult
  class func rotateBy(x xAngle: CGFloat, y yAngle: CGFloat, z zAngle: CGFloat, duration duration: NSTimeInterval) -> SCNAction
  @discardableResult
  class func rotateTo(x xAngle: CGFloat, y yAngle: CGFloat, z zAngle: CGFloat, duration duration: NSTimeInterval) -> SCNAction
  @discardableResult
  class func rotateTo(x xAngle: CGFloat, y yAngle: CGFloat, z zAngle: CGFloat, duration duration: NSTimeInterval, shortestUnitArc shortestUnitArc: Bool) -> SCNAction
  @discardableResult
  class func rotate(byAngle angle: CGFloat, aroundAxis axis: SCNVector3, duration duration: NSTimeInterval) -> SCNAction
  @discardableResult
  class func rotate(toAxisAngle axisAngle: SCNVector4, duration duration: NSTimeInterval) -> SCNAction
  @discardableResult
  class func scale(by scale: CGFloat, duration sec: NSTimeInterval) -> SCNAction
  @discardableResult
  class func scale(to scale: CGFloat, duration sec: NSTimeInterval) -> SCNAction
  @discardableResult
  class func sequence(_ actions: [SCNAction]) -> SCNAction
  @discardableResult
  class func group(_ actions: [SCNAction]) -> SCNAction
  @discardableResult
  class func repeatAction(_ action: SCNAction, count count: Int) -> SCNAction
  @discardableResult
  class func repeatForever(_ action: SCNAction) -> SCNAction
  @discardableResult
  class func fadeIn(withDuration sec: NSTimeInterval) -> SCNAction
  @discardableResult
  class func fadeOut(withDuration sec: NSTimeInterval) -> SCNAction
  @discardableResult
  class func fadeOpacity(by factor: CGFloat, duration sec: NSTimeInterval) -> SCNAction
  @discardableResult
  class func fadeOpacity(to opacity: CGFloat, duration sec: NSTimeInterval) -> SCNAction
  @available(tvOS 9.0, *)
  @discardableResult
  class func hide() -> SCNAction
  @available(tvOS 9.0, *)
  @discardableResult
  class func unhide() -> SCNAction
  @discardableResult
  class func wait(forDuration sec: NSTimeInterval) -> SCNAction
  @discardableResult
  class func wait(forDuration sec: NSTimeInterval, withRange durationRange: NSTimeInterval) -> SCNAction
  @discardableResult
  class func removeFromParentNode() -> SCNAction
  @discardableResult
  class func run(_ block: (SCNNode) -> Void) -> SCNAction
  @discardableResult
  class func run(_ block: (SCNNode) -> Void, queue queue: dispatch_queue_t) -> SCNAction
  @discardableResult
  class func javaScriptAction(withScript script: String, duration seconds: NSTimeInterval) -> SCNAction
  @discardableResult
  class func customAction(withDuration seconds: NSTimeInterval, actionBlock block: (SCNNode, CGFloat) -> Void) -> SCNAction
  @available(tvOS 9.0, *)
  @discardableResult
  class func play(_ source: SCNAudioSource, waitForCompletion wait: Bool) -> SCNAction
}
