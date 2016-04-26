
@available(iOS 8.0, *)
enum SCNPhysicsBodyType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `static`
  case dynamic
  case kinematic
}
@available(iOS 8.0, *)
struct SCNPhysicsCollisionCategory : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var `default`: SCNPhysicsCollisionCategory { get }
  static var `static`: SCNPhysicsCollisionCategory { get }
  static var all: SCNPhysicsCollisionCategory { get }
}
@available(iOS 8.0, *)
class SCNPhysicsBody : NSObject, NSCopying, NSSecureCoding {
  @discardableResult
  class func staticBody() -> Self
  @discardableResult
  class func dynamic() -> Self
  @discardableResult
  class func kinematic() -> Self
  convenience init(type type: SCNPhysicsBodyType, shape shape: SCNPhysicsShape?)
  var type: SCNPhysicsBodyType
  var mass: CGFloat
  @available(iOS 9.0, *)
  var momentOfInertia: SCNVector3
  @available(iOS 9.0, *)
  var usesDefaultMomentOfInertia: Bool
  var charge: CGFloat
  var friction: CGFloat
  var restitution: CGFloat
  var rollingFriction: CGFloat
  var physicsShape: SCNPhysicsShape?
  var isResting: Bool { get }
  var allowsResting: Bool
  var velocity: SCNVector3
  var angularVelocity: SCNVector4
  var damping: CGFloat
  var angularDamping: CGFloat
  var velocityFactor: SCNVector3
  var angularVelocityFactor: SCNVector3
  var categoryBitMask: Int
  var collisionBitMask: Int
  @available(iOS 9.0, *)
  var contactTestBitMask: Int
  @available(iOS 9.0, *)
  var isAffectedByGravity: Bool
  func applyForce(_ direction: SCNVector3, impulse impulse: Bool)
  func applyForce(_ direction: SCNVector3, atPosition position: SCNVector3, impulse impulse: Bool)
  func applyTorque(_ torque: SCNVector4, impulse impulse: Bool)
  func clearAllForces()
  func resetTransform()
}
