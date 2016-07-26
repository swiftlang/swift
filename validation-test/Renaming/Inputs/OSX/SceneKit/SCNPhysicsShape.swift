
@available(OSX 10.10, *)
let SCNPhysicsShapeTypeKey: String
@available(OSX 10.10, *)
let SCNPhysicsShapeTypeBoundingBox: String
@available(OSX 10.10, *)
let SCNPhysicsShapeTypeConvexHull: String
@available(OSX 10.10, *)
let SCNPhysicsShapeTypeConcavePolyhedron: String
@available(OSX 10.10, *)
let SCNPhysicsShapeKeepAsCompoundKey: String
@available(OSX 10.10, *)
let SCNPhysicsShapeScaleKey: String
@available(OSX 10.10, *)
class SCNPhysicsShape : NSObject, NSCopying, NSSecureCoding {
  convenience init(geometry geometry: SCNGeometry, options options: [String : AnyObject]? = [:])
  convenience init(node node: SCNNode, options options: [String : AnyObject]? = [:])
  convenience init(shapes shapes: [SCNPhysicsShape], transforms transforms: [NSValue]?)
  @available(OSX 10.11, *)
  var options: [String : AnyObject]? { get }
  @available(OSX 10.11, *)
  var sourceObject: AnyObject { get }
  @available(OSX 10.11, *)
  var transforms: [NSValue]? { get }
}
