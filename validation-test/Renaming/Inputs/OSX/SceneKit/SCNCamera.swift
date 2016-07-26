
@available(OSX 10.8, *)
class SCNCamera : NSObject, SCNAnimatable, SCNTechniqueSupport, NSCopying, NSSecureCoding {
  var name: String?
  var xFov: Double
  var yFov: Double
  var zNear: Double
  var zFar: Double
  @available(OSX 10.9, *)
  var automaticallyAdjustsZRange: Bool
  var usesOrthographicProjection: Bool
  @available(OSX 10.9, *)
  var orthographicScale: Double
  @discardableResult
  func projectionTransform() -> SCNMatrix4
  @available(OSX 10.9, *)
  func setProjectionTransform(_ projectionTransform: SCNMatrix4)
  @available(OSX 10.9, *)
  var focalDistance: CGFloat
  @available(OSX 10.9, *)
  var focalSize: CGFloat
  @available(OSX 10.9, *)
  var focalBlurRadius: CGFloat
  @available(OSX 10.9, *)
  var aperture: CGFloat
  @available(OSX 10.10, *)
  var categoryBitMask: Int
}
