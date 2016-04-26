
@available(tvOS 8.0, *)
let SCNPhysicsShapeTypeKey: String
@available(tvOS 8.0, *)
let SCNPhysicsShapeTypeBoundingBox: String
@available(tvOS 8.0, *)
let SCNPhysicsShapeTypeConvexHull: String
@available(tvOS 8.0, *)
let SCNPhysicsShapeTypeConcavePolyhedron: String
@available(tvOS 8.0, *)
let SCNPhysicsShapeKeepAsCompoundKey: String
@available(tvOS 8.0, *)
let SCNPhysicsShapeScaleKey: String
@available(tvOS 8.0, *)
class SCNPhysicsShape : NSObject, NSCopying, NSSecureCoding {
  convenience init(geometry geometry: SCNGeometry, options options: [String : AnyObject]? = [:])
  convenience init(node node: SCNNode, options options: [String : AnyObject]? = [:])
  convenience init(shapes shapes: [SCNPhysicsShape], transforms transforms: [NSValue]?)
  @available(tvOS 9.0, *)
  var options: [String : AnyObject]? { get }
  @available(tvOS 9.0, *)
  var sourceObject: AnyObject { get }
  @available(tvOS 9.0, *)
  var transforms: [NSValue]? { get }
}
