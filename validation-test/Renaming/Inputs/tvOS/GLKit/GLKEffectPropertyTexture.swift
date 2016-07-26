
@available(tvOS 5.0, *)
enum GLKTextureTarget : GLenum {
  init?(rawValue rawValue: GLenum)
  var rawValue: GLenum { get }
  case target2D
  case targetCubeMap
  case targetCt
}
@available(tvOS 5.0, *)
enum GLKTextureEnvMode : GLint {
  init?(rawValue rawValue: GLint)
  var rawValue: GLint { get }
  case replace
  case modulate
  case decal
}
@available(tvOS 5.0, *)
class GLKEffectPropertyTexture : GLKEffectProperty {
  var enabled: GLboolean
  var name: GLuint
  var target: GLKTextureTarget
  var envMode: GLKTextureEnvMode
}
