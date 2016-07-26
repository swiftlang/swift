
@available(iOS 8.0, *)
let SCNPhysicsTestCollisionBitMaskKey: String
@available(iOS 8.0, *)
let SCNPhysicsTestSearchModeKey: String
@available(iOS 8.0, *)
let SCNPhysicsTestBackfaceCullingKey: String
@available(iOS 8.0, *)
let SCNPhysicsTestSearchModeAny: String
@available(iOS 8.0, *)
let SCNPhysicsTestSearchModeClosest: String
@available(iOS 8.0, *)
let SCNPhysicsTestSearchModeAll: String
@available(iOS 8.0, *)
protocol SCNPhysicsContactDelegate : NSObjectProtocol {
  optional func physicsWorld(_ world: SCNPhysicsWorld, didBegin contact: SCNPhysicsContact)
  optional func physicsWorld(_ world: SCNPhysicsWorld, didUpdate contact: SCNPhysicsContact)
  optional func physicsWorld(_ world: SCNPhysicsWorld, didEnd contact: SCNPhysicsContact)
}
@available(iOS 8.0, *)
class SCNPhysicsWorld : NSObject, NSSecureCoding {
  var gravity: SCNVector3
  var speed: CGFloat
  var timeStep: NSTimeInterval
  unowned(unsafe) var contactDelegate: @sil_unmanaged SCNPhysicsContactDelegate?
  func add(_ behavior: SCNPhysicsBehavior)
  func remove(_ behavior: SCNPhysicsBehavior)
  func removeAllBehaviors()
  var allBehaviors: [SCNPhysicsBehavior] { get }
  @discardableResult
  func rayTestWithSegment(fromPoint origin: SCNVector3, toPoint dest: SCNVector3, options options: [String : AnyObject]? = [:]) -> [SCNHitTestResult]
  @discardableResult
  func contactTestBetweenBody(_ bodyA: SCNPhysicsBody, andBody bodyB: SCNPhysicsBody, options options: [String : AnyObject]? = [:]) -> [SCNPhysicsContact]
  @discardableResult
  func contactTest(with body: SCNPhysicsBody, options options: [String : AnyObject]? = [:]) -> [SCNPhysicsContact]
  @discardableResult
  func convexSweepTest(with shape: SCNPhysicsShape, fromTransform from: SCNMatrix4, toTransform to: SCNMatrix4, options options: [String : AnyObject]? = [:]) -> [SCNPhysicsContact]
  func updateCollisionPairs()
}
