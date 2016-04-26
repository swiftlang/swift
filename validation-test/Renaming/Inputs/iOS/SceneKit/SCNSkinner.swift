
@available(iOS 8.0, *)
class SCNSkinner : NSObject, NSSecureCoding {
  var skeleton: SCNNode?
  @available(iOS 8.0, *)
  convenience init(baseGeometry baseGeometry: SCNGeometry?, bones bones: [SCNNode], boneInverseBindTransforms boneInverseBindTransforms: [NSValue]?, boneWeights boneWeights: SCNGeometrySource, boneIndices boneIndices: SCNGeometrySource)
  @available(iOS 8.0, *)
  var baseGeometry: SCNGeometry?
  @available(iOS 8.0, *)
  var baseGeometryBindTransform: SCNMatrix4
  @available(iOS 8.0, *)
  var boneInverseBindTransforms: [NSValue]? { get }
  @available(iOS 8.0, *)
  var bones: [SCNNode] { get }
  @available(iOS 8.0, *)
  var boneWeights: SCNGeometrySource { get }
  @available(iOS 8.0, *)
  var boneIndices: SCNGeometrySource { get }
}
