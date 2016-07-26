
@available(OSX 10.9, *)
class SCNSkinner : NSObject, NSSecureCoding {
  var skeleton: SCNNode?
  @available(OSX 10.10, *)
  convenience init(baseGeometry baseGeometry: SCNGeometry?, bones bones: [SCNNode], boneInverseBindTransforms boneInverseBindTransforms: [NSValue]?, boneWeights boneWeights: SCNGeometrySource, boneIndices boneIndices: SCNGeometrySource)
  @available(OSX 10.9, *)
  var baseGeometry: SCNGeometry?
  @available(OSX 10.10, *)
  var baseGeometryBindTransform: SCNMatrix4
  @available(OSX 10.10, *)
  var boneInverseBindTransforms: [NSValue]? { get }
  @available(OSX 10.10, *)
  var bones: [SCNNode] { get }
  @available(OSX 10.10, *)
  var boneWeights: SCNGeometrySource { get }
  @available(OSX 10.10, *)
  var boneIndices: SCNGeometrySource { get }
}
