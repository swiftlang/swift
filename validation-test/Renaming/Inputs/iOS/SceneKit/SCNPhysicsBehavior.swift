
@available(iOS 8.0, *)
class SCNPhysicsBehavior : NSObject, NSSecureCoding {
}
@available(iOS 8.0, *)
class SCNPhysicsHingeJoint : SCNPhysicsBehavior {
  convenience init(bodyA bodyA: SCNPhysicsBody, axisA axisA: SCNVector3, anchorA anchorA: SCNVector3, bodyB bodyB: SCNPhysicsBody, axisB axisB: SCNVector3, anchorB anchorB: SCNVector3)
  convenience init(body body: SCNPhysicsBody, axis axis: SCNVector3, anchor anchor: SCNVector3)
  var bodyA: SCNPhysicsBody { get }
  var axisA: SCNVector3
  var anchorA: SCNVector3
  var bodyB: SCNPhysicsBody? { get }
  var axisB: SCNVector3
  var anchorB: SCNVector3
}
@available(iOS 8.0, *)
class SCNPhysicsBallSocketJoint : SCNPhysicsBehavior {
  convenience init(bodyA bodyA: SCNPhysicsBody, anchorA anchorA: SCNVector3, bodyB bodyB: SCNPhysicsBody, anchorB anchorB: SCNVector3)
  convenience init(body body: SCNPhysicsBody, anchor anchor: SCNVector3)
  var bodyA: SCNPhysicsBody { get }
  var anchorA: SCNVector3
  var bodyB: SCNPhysicsBody? { get }
  var anchorB: SCNVector3
}
@available(iOS 8.0, *)
class SCNPhysicsSliderJoint : SCNPhysicsBehavior {
  convenience init(bodyA bodyA: SCNPhysicsBody, axisA axisA: SCNVector3, anchorA anchorA: SCNVector3, bodyB bodyB: SCNPhysicsBody, axisB axisB: SCNVector3, anchorB anchorB: SCNVector3)
  convenience init(body body: SCNPhysicsBody, axis axis: SCNVector3, anchor anchor: SCNVector3)
  var bodyA: SCNPhysicsBody { get }
  var axisA: SCNVector3
  var anchorA: SCNVector3
  var bodyB: SCNPhysicsBody? { get }
  var axisB: SCNVector3
  var anchorB: SCNVector3
  var minimumLinearLimit: CGFloat
  var maximumLinearLimit: CGFloat
  var minimumAngularLimit: CGFloat
  var maximumAngularLimit: CGFloat
  var motorTargetLinearVelocity: CGFloat
  var motorMaximumForce: CGFloat
  var motorTargetAngularVelocity: CGFloat
  var motorMaximumTorque: CGFloat
}
@available(iOS 8.0, *)
class SCNPhysicsVehicleWheel : NSObject, NSCopying, NSSecureCoding {
  convenience init(node node: SCNNode)
  var node: SCNNode { get }
  var suspensionStiffness: CGFloat
  var suspensionCompression: CGFloat
  var suspensionDamping: CGFloat
  var maximumSuspensionTravel: CGFloat
  var frictionSlip: CGFloat
  var maximumSuspensionForce: CGFloat
  var connectionPosition: SCNVector3
  var steeringAxis: SCNVector3
  var axle: SCNVector3
  var radius: CGFloat
  var suspensionRestLength: CGFloat
}
@available(iOS 8.0, *)
class SCNPhysicsVehicle : SCNPhysicsBehavior {
  convenience init(chassisBody chassisBody: SCNPhysicsBody, wheels wheels: [SCNPhysicsVehicleWheel])
  var speedInKilometersPerHour: CGFloat { get }
  var wheels: [SCNPhysicsVehicleWheel] { get }
  var chassisBody: SCNPhysicsBody { get }
  func applyEngineForce(_ value: CGFloat, forWheelAt index: Int)
  func setSteeringAngle(_ value: CGFloat, forWheelAt index: Int)
  func applyBrakingForce(_ value: CGFloat, forWheelAt index: Int)
}
