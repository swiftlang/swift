
@available(OSX 10.9, *)
class SCNConstraint : NSObject, NSCopying, NSSecureCoding, SCNAnimatable {
  @available(OSX 10.10, *)
  var influenceFactor: CGFloat
}
@available(OSX 10.9, *)
class SCNLookAtConstraint : SCNConstraint {
  convenience init(target target: SCNNode)
  var target: SCNNode { get }
  var gimbalLockEnabled: Bool
}
struct SCNBillboardAxis : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var X: SCNBillboardAxis { get }
  static var Y: SCNBillboardAxis { get }
  static var Z: SCNBillboardAxis { get }
  static var all: SCNBillboardAxis { get }
}
@available(OSX 10.11, *)
class SCNBillboardConstraint : SCNConstraint {
  var freeAxes: SCNBillboardAxis
}
@available(OSX 10.9, *)
class SCNTransformConstraint : SCNConstraint {
  convenience init(inWorldSpace world: Bool, with block: (SCNNode, SCNMatrix4) -> SCNMatrix4)
}
@available(OSX 10.10, *)
class SCNIKConstraint : SCNConstraint {
  @available(OSX 10.11, *)
  init(chainRootNode chainRootNode: SCNNode)
  @discardableResult
  class func inverseKinematicsConstraint(withChainRootNode chainRootNode: SCNNode) -> Self
  var chainRootNode: SCNNode { get }
  var targetPosition: SCNVector3
  func setMaxAllowedRotationAngle(_ angle: CGFloat, forJoint node: SCNNode)
  @discardableResult
  func maxAllowedRotationAngle(forJoint node: SCNNode) -> CGFloat
}
