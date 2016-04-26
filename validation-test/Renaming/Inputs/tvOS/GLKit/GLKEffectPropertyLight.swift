
@available(tvOS 5.0, *)
enum GLKLightingType : GLint {
  init?(rawValue rawValue: GLint)
  var rawValue: GLint { get }
  case perVertex
  case perPixel
}
@available(tvOS 5.0, *)
class GLKEffectPropertyLight : GLKEffectProperty {
  var enabled: GLboolean
  var position: GLKVector4
  var ambientColor: GLKVector4
  var diffuseColor: GLKVector4
  var specularColor: GLKVector4
  var spotDirection: GLKVector3
  var spotExponent: GLfloat
  var spotCutoff: GLfloat
  var constantAttenuation: GLfloat
  var linearAttenuation: GLfloat
  var quadraticAttenuation: GLfloat
  var transform: GLKEffectPropertyTransform
}
