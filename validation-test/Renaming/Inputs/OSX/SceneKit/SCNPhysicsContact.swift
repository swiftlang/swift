
@available(OSX 10.10, *)
class SCNPhysicsContact : NSObject {
  var nodeA: SCNNode { get }
  var nodeB: SCNNode { get }
  var contactPoint: SCNVector3 { get }
  var contactNormal: SCNVector3 { get }
  var collisionImpulse: CGFloat { get }
  var penetrationDistance: CGFloat { get }
}
