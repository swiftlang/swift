
@available(iOS 5.0, *)
class GLKSkyboxEffect : NSObject, GLKNamedEffect {
  func draw()
  var center: GLKVector3
  var xSize: GLfloat
  var ySize: GLfloat
  var zSize: GLfloat
  var textureCubeMap: GLKEffectPropertyTexture { get }
  var transform: GLKEffectPropertyTransform { get }
  var label: String?
}
