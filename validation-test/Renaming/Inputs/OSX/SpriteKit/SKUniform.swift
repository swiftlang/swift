
@available(OSX 10.10, *)
enum SKUniformType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case float
  case floatVector2
  case floatVector3
  case floatVector4
  case floatMatrix2
  case floatMatrix3
  case floatMatrix4
  case texture
}
@available(OSX 10.10, *)
class SKUniform : NSObject, NSCopying, NSCoding {
  convenience init(name name: String, texture texture: SKTexture)
  var name: String { get }
  var uniformType: SKUniformType { get }
  var textureValue: SKTexture?
  var floatValue: Float
  var floatVector2Value: GLKVector2
  var floatVector3Value: GLKVector3
  var floatVector4Value: GLKVector4
  var floatMatrix2Value: GLKMatrix2
  var floatMatrix3Value: GLKMatrix3
  var floatMatrix4Value: GLKMatrix4
  init(name name: String)
  init(name name: String, texture texture: SKTexture?)
  init(name name: String, float value: Float)
  init(name name: String, float value: GLKVector2)
  init(name name: String, float value: GLKVector3)
  init(name name: String, float value: GLKVector4)
  init(name name: String, float value: GLKMatrix2)
  init(name name: String, float value: GLKMatrix3)
  init(name name: String, float value: GLKMatrix4)
}
