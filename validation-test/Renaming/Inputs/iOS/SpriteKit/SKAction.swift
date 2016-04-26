
@available(iOS 7.0, *)
enum SKActionTimingMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case linear
  case easeIn
  case easeOut
  case easeInEaseOut
}
typealias SKActionTimingFunction = (Float) -> Float
class SKAction : NSObject, NSCopying, NSCoding {
  var duration: NSTimeInterval
  var timingMode: SKActionTimingMode
  @available(iOS 8.0, *)
  var timingFunction: SKActionTimingFunction
  var speed: CGFloat
  @discardableResult
  func reversed() -> SKAction
}
extension SKAction {
  @discardableResult
  class func move(by delta: CGVector, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func moveBy(x deltaX: CGFloat, y deltaY: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func move(to location: CGPoint, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func moveTo(x x: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func moveTo(y y: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func rotate(byAngle radians: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func rotate(toAngle radians: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func rotate(toAngle radians: CGFloat, duration sec: NSTimeInterval, shortestUnitArc shortestUnitArc: Bool) -> SKAction
  @discardableResult
  class func resize(byWidth width: CGFloat, height height: CGFloat, duration duration: NSTimeInterval) -> SKAction
  @discardableResult
  class func resize(toWidth width: CGFloat, height height: CGFloat, duration duration: NSTimeInterval) -> SKAction
  @discardableResult
  class func resize(toWidth width: CGFloat, duration duration: NSTimeInterval) -> SKAction
  @discardableResult
  class func resize(toHeight height: CGFloat, duration duration: NSTimeInterval) -> SKAction
  @discardableResult
  class func scale(by scale: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func scaleX(by xScale: CGFloat, y yScale: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func scale(to scale: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func scaleX(to xScale: CGFloat, y yScale: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func scaleX(to scale: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func scaleY(to scale: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func sequence(_ actions: [SKAction]) -> SKAction
  @discardableResult
  class func group(_ actions: [SKAction]) -> SKAction
  @discardableResult
  class func repeatAction(_ action: SKAction, count count: Int) -> SKAction
  @discardableResult
  class func repeatForever(_ action: SKAction) -> SKAction
  @discardableResult
  class func fadeIn(withDuration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func fadeOut(withDuration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func fadeAlpha(by factor: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func fadeAlpha(to alpha: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @available(iOS 8.0, *)
  @discardableResult
  class func hide() -> SKAction
  @available(iOS 8.0, *)
  @discardableResult
  class func unhide() -> SKAction
  @available(iOS 7.1, *)
  @discardableResult
  class func setTexture(_ texture: SKTexture) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func setNormalTexture(_ texture: SKTexture) -> SKAction
  @available(iOS 7.1, *)
  @discardableResult
  class func setTexture(_ texture: SKTexture, resize resize: Bool) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func setNormalTexture(_ texture: SKTexture, resize resize: Bool) -> SKAction
  @discardableResult
  class func animate(with textures: [SKTexture], timePerFrame sec: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func animate(withNormalTextures textures: [SKTexture], timePerFrame sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func animate(with textures: [SKTexture], timePerFrame sec: NSTimeInterval, resize resize: Bool, restore restore: Bool) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func animate(withNormalTextures textures: [SKTexture], timePerFrame sec: NSTimeInterval, resize resize: Bool, restore restore: Bool) -> SKAction
  @discardableResult
  class func playSoundFileNamed(_ soundFile: String, waitForCompletion wait: Bool) -> SKAction
  @discardableResult
  class func colorize(with color: UIColor, colorBlendFactor colorBlendFactor: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func colorize(withColorBlendFactor colorBlendFactor: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @available(iOS 8.0, *)
  @discardableResult
  class func falloff(to falloff: Float, duration sec: NSTimeInterval) -> SKAction
  @available(iOS 8.0, *)
  @discardableResult
  class func falloff(by falloff: Float, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func follow(_ path: CGPath, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func follow(_ path: CGPath, asOffset offset: Bool, orientToPath orient: Bool, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func follow(_ path: CGPath, speed speed: CGFloat) -> SKAction
  @discardableResult
  class func follow(_ path: CGPath, asOffset offset: Bool, orientToPath orient: Bool, speed speed: CGFloat) -> SKAction
  @discardableResult
  class func speed(by speed: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func speed(to speed: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @available(iOS 8.0, *)
  @discardableResult
  class func reach(to position: CGPoint, rootNode root: SKNode, duration sec: NSTimeInterval) -> SKAction
  @available(iOS 8.0, *)
  @discardableResult
  class func reach(to position: CGPoint, rootNode root: SKNode, velocity velocity: CGFloat) -> SKAction
  @available(iOS 8.0, *)
  @discardableResult
  class func reach(to node: SKNode, rootNode root: SKNode, duration sec: NSTimeInterval) -> SKAction
  @available(iOS 8.0, *)
  @discardableResult
  class func reach(to node: SKNode, rootNode root: SKNode, velocity velocity: CGFloat) -> SKAction
  @available(iOS 8.0, *)
  @discardableResult
  class func strength(to strength: Float, duration sec: NSTimeInterval) -> SKAction
  @available(iOS 8.0, *)
  @discardableResult
  class func strength(by strength: Float, duration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func wait(forDuration sec: NSTimeInterval) -> SKAction
  @discardableResult
  class func wait(forDuration sec: NSTimeInterval, withRange durationRange: NSTimeInterval) -> SKAction
  @discardableResult
  class func removeFromParent() -> SKAction
  @discardableResult
  class func perform(_ selector: Selector, onTarget target: AnyObject) -> SKAction
  @discardableResult
  class func run(_ block: dispatch_block_t) -> SKAction
  @discardableResult
  class func run(_ block: dispatch_block_t, queue queue: dispatch_queue_t) -> SKAction
  @discardableResult
  class func run(_ action: SKAction, onChildWithName name: String) -> SKAction
  @discardableResult
  class func customAction(withDuration seconds: NSTimeInterval, actionBlock block: (SKNode, CGFloat) -> Void) -> SKAction
  @available(iOS 9.0, *)
  /*not inherited*/ init?(named name: String)
  @available(iOS 9.0, *)
  /*not inherited*/ init?(named name: String, duration sec: NSTimeInterval)
  @available(iOS 9.0, *)
  /*not inherited*/ init?(named name: String, from url: NSURL)
  @available(iOS 9.0, *)
  /*not inherited*/ init?(named name: String, from url: NSURL, duration sec: NSTimeInterval)
}
extension SKAction {
  @available(iOS 9.0, *)
  @discardableResult
  class func changeCharge(to v: Float, duration duration: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func changeCharge(by v: Float, duration duration: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func changeMass(to v: Float, duration duration: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func changeMass(by v: Float, duration duration: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func applyForce(_ force: CGVector, duration sec: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func applyForce(_ force: CGVector, at point: CGPoint, duration sec: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func applyTorque(_ torque: CGFloat, duration sec: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func applyImpulse(_ impulse: CGVector, duration sec: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func applyImpulse(_ impulse: CGVector, at point: CGPoint, duration sec: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func applyAngularImpulse(_ impulse: CGFloat, duration sec: NSTimeInterval) -> SKAction
}
extension SKAction {
  @available(iOS 9.0, *)
  @discardableResult
  class func play() -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func pause() -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func stop() -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func changePlaybackRate(to v: Float, duration duration: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func changePlaybackRate(by v: Float, duration duration: NSTimeInterval) -> SKAction
}
extension SKAction {
  @available(iOS 9.0, *)
  @discardableResult
  class func changeVolume(to v: Float, duration duration: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func changeVolume(by v: Float, duration duration: NSTimeInterval) -> SKAction
}
