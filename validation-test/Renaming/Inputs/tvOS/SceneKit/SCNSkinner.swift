
@available(tvOS 8.0, *)
class SCNSkinner : NSObject, NSSecureCoding {
  var skeleton: SCNNode?
  @available(tvOS 8.0, *)
  convenience init(baseGeometry baseGeometry: SCNGeometry?, bones bones: [SCNNode], boneInverseBindTransforms boneInverseBindTransforms: [NSValue]?, boneWeights boneWeights: SCNGeometrySource, boneIndices boneIndices: SCNGeometrySource)
  @available(tvOS 8.0, *)
  var baseGeometry: SCNGeometry?
  @available(tvOS 8.0, *)
  var baseGeometryBindTransform: SCNMatrix4
  @available(tvOS 8.0, *)
  var boneInverseBindTransforms: [NSValue]? { get }
  @available(tvOS 8.0, *)
  var bones: [SCNNode] { get }
  @available(tvOS 8.0, *)
  var boneWeights: SCNGeometrySource { get }
  @available(tvOS 8.0, *)
  var boneIndices: SCNGeometrySource { get }
}
