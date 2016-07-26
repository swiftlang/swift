
protocol SKPhysicsContactDelegate : NSObjectProtocol {
  optional func didBegin(_ contact: SKPhysicsContact)
  optional func didEnd(_ contact: SKPhysicsContact)
}
class SKPhysicsWorld : NSObject, NSCoding {
  var gravity: CGVector
  var speed: CGFloat
  unowned(unsafe) var contactDelegate: @sil_unmanaged SKPhysicsContactDelegate?
  func add(_ joint: SKPhysicsJoint)
  func remove(_ joint: SKPhysicsJoint)
  func removeAllJoints()
  @available(tvOS 8.0, *)
  @discardableResult
  func sampleFields(at position: vector_float3) -> vector_float3
  @discardableResult
  func body(at point: CGPoint) -> SKPhysicsBody?
  @discardableResult
  func body(in rect: CGRect) -> SKPhysicsBody?
  @discardableResult
  func body(alongRayStart start: CGPoint, end end: CGPoint) -> SKPhysicsBody?
  func enumerateBodies(at point: CGPoint, using block: (SKPhysicsBody, UnsafeMutablePointer<ObjCBool>) -> Void)
  func enumerateBodies(in rect: CGRect, using block: (SKPhysicsBody, UnsafeMutablePointer<ObjCBool>) -> Void)
  func enumerateBodies(alongRayStart start: CGPoint, end end: CGPoint, using block: (SKPhysicsBody, CGPoint, CGVector, UnsafeMutablePointer<ObjCBool>) -> Void)
}
