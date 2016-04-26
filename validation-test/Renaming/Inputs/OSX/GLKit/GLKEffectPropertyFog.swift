
@available(OSX 10.8, *)
enum GLKFogMode : GLint {
  init?(rawValue rawValue: GLint)
  var rawValue: GLint { get }
  case exp
  case exp2
  case linear
}
@available(OSX 10.8, *)
class GLKEffectPropertyFog : GLKEffectProperty {
  var enabled: GLboolean
  var mode: GLint
  var color: GLKVector4
  var density: GLfloat
  var start: GLfloat
  var end: GLfloat
}
