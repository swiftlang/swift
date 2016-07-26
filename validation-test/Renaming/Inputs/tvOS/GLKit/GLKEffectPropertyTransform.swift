
@available(tvOS 5.0, *)
class GLKEffectPropertyTransform : GLKEffectProperty {
  var modelviewMatrix: GLKMatrix4
  var projectionMatrix: GLKMatrix4
  var normalMatrix: GLKMatrix3 { get }
}
