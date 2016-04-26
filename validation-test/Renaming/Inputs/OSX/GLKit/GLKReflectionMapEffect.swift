
@available(OSX 10.8, *)
class GLKReflectionMapEffect : GLKBaseEffect, GLKNamedEffect {
  var textureCubeMap: GLKEffectPropertyTexture { get }
  var matrix: GLKMatrix3
}
