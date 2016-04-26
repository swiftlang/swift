
@available(tvOS 8.0, *)
class SCNCamera : NSObject, SCNAnimatable, SCNTechniqueSupport, NSCopying, NSSecureCoding {
  var name: String?
  var xFov: Double
  var yFov: Double
  var zNear: Double
  var zFar: Double
  @available(tvOS 8.0, *)
  var automaticallyAdjustsZRange: Bool
  var usesOrthographicProjection: Bool
  @available(tvOS 8.0, *)
  var orthographicScale: Double
  @discardableResult
  func projectionTransform() -> SCNMatrix4
  @available(tvOS 8.0, *)
  func setProjectionTransform(_ projectionTransform: SCNMatrix4)
  @available(tvOS 8.0, *)
  var focalDistance: CGFloat
  @available(tvOS 8.0, *)
  var focalSize: CGFloat
  @available(tvOS 8.0, *)
  var focalBlurRadius: CGFloat
  @available(tvOS 8.0, *)
  var aperture: CGFloat
  @available(tvOS 8.0, *)
  var categoryBitMask: Int
}
