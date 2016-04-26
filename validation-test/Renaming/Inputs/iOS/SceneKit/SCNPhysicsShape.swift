
@available(iOS 8.0, *)
let SCNPhysicsShapeTypeKey: String
@available(iOS 8.0, *)
let SCNPhysicsShapeTypeBoundingBox: String
@available(iOS 8.0, *)
let SCNPhysicsShapeTypeConvexHull: String
@available(iOS 8.0, *)
let SCNPhysicsShapeTypeConcavePolyhedron: String
@available(iOS 8.0, *)
let SCNPhysicsShapeKeepAsCompoundKey: String
@available(iOS 8.0, *)
let SCNPhysicsShapeScaleKey: String
@available(iOS 8.0, *)
class SCNPhysicsShape : NSObject, NSCopying, NSSecureCoding {
  convenience init(geometry geometry: SCNGeometry, options options: [String : AnyObject]? = [:])
  convenience init(node node: SCNNode, options options: [String : AnyObject]? = [:])
  convenience init(shapes shapes: [SCNPhysicsShape], transforms transforms: [NSValue]?)
  @available(iOS 9.0, *)
  var options: [String : AnyObject]? { get }
  @available(iOS 9.0, *)
  var sourceObject: AnyObject { get }
  @available(iOS 9.0, *)
  var transforms: [NSValue]? { get }
}
