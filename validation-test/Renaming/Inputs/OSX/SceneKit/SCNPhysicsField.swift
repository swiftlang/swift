
@available(OSX 10.10, *)
enum SCNPhysicsFieldScope : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case insideExtent
  case outsideExtent
}
@available(OSX 10.10, *)
class SCNPhysicsField : NSObject, NSCopying, NSSecureCoding {
  var strength: CGFloat
  var falloffExponent: CGFloat
  var minimumDistance: CGFloat
  var isActive: Bool
  var isExclusive: Bool
  var halfExtent: SCNVector3
  var usesEllipsoidalExtent: Bool
  var scope: SCNPhysicsFieldScope
  var offset: SCNVector3
  var direction: SCNVector3
  @available(OSX 10.10, *)
  var categoryBitMask: Int
  @discardableResult
  class func drag() -> SCNPhysicsField
  @discardableResult
  class func vortex() -> SCNPhysicsField
  @discardableResult
  class func radialGravity() -> SCNPhysicsField
  @discardableResult
  class func linearGravity() -> SCNPhysicsField
  @discardableResult
  class func noiseField(withSmoothness smoothness: CGFloat, animationSpeed speed: CGFloat) -> SCNPhysicsField
  @discardableResult
  class func turbulenceField(withSmoothness smoothness: CGFloat, animationSpeed speed: CGFloat) -> SCNPhysicsField
  @discardableResult
  class func spring() -> SCNPhysicsField
  @discardableResult
  class func electric() -> SCNPhysicsField
  @discardableResult
  class func magnetic() -> SCNPhysicsField
  @discardableResult
  class func customField(evaluationBlock block: SCNFieldForceEvaluator) -> SCNPhysicsField
}
typealias SCNFieldForceEvaluator = (SCNVector3, SCNVector3, Float, Float, NSTimeInterval) -> SCNVector3
