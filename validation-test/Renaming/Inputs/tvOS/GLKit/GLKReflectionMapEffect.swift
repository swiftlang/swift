
@available(tvOS 5.0, *)
class GLKReflectionMapEffect : GLKBaseEffect, GLKNamedEffect {
  var textureCubeMap: GLKEffectPropertyTexture { get }
  var matrix: GLKMatrix3
}
