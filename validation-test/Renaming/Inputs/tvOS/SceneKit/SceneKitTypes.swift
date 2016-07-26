
struct SCNVector3 {
  var x: Float
  var y: Float
  var z: Float
  init()
  init(x x: Float, y y: Float, z z: Float)
}

extension SCNVector3 {
  init(_ x: Float, _ y: Float, _ z: Float)
  init(_ x: CGFloat, _ y: CGFloat, _ z: CGFloat)
  init(_ x: Double, _ y: Double, _ z: Double)
  init(_ x: Int, _ y: Int, _ z: Int)
  init(_ v: float3)
  init(_ v: double3)
}
struct SCNVector4 {
  var x: Float
  var y: Float
  var z: Float
  var w: Float
  init()
  init(x x: Float, y y: Float, z z: Float, w w: Float)
}

extension SCNVector4 {
  init(_ x: Float, _ y: Float, _ z: Float, _ w: Float)
  init(_ x: CGFloat, _ y: CGFloat, _ z: CGFloat, _ w: CGFloat)
  init(_ x: Double, _ y: Double, _ z: Double, _ w: Double)
  init(_ x: Int, _ y: Int, _ z: Int, _ w: Int)
  init(_ v: float4)
  init(_ v: double4)
}
struct SCNMatrix4 {
  var m11: Float
  var m12: Float
  var m13: Float
  var m14: Float
  var m21: Float
  var m22: Float
  var m23: Float
  var m24: Float
  var m31: Float
  var m32: Float
  var m33: Float
  var m34: Float
  var m41: Float
  var m42: Float
  var m43: Float
  var m44: Float
  init()
  init(m11 m11: Float, m12 m12: Float, m13 m13: Float, m14 m14: Float, m21 m21: Float, m22 m22: Float, m23 m23: Float, m24 m24: Float, m31 m31: Float, m32 m32: Float, m33 m33: Float, m34 m34: Float, m41 m41: Float, m42 m42: Float, m43 m43: Float, m44 m44: Float)
}

extension SCNMatrix4 {
  init(_ m: float4x4)
  init(_ m: double4x4)
}
typealias SCNQuaternion = SCNVector4
@available(tvOS 8.0, *)
let SCNMatrix4Identity: SCNMatrix4
@available(tvOS 8.0, *)
let SCNVector3Zero: SCNVector3
@available(tvOS 8.0, *)
let SCNVector4Zero: SCNVector4
@discardableResult
func SCNVector3EqualToVector3(_ a: SCNVector3, _ b: SCNVector3) -> Bool
@discardableResult
func SCNVector4EqualToVector4(_ a: SCNVector4, _ b: SCNVector4) -> Bool
@discardableResult
func SCNVector3Make(_ x: Float, _ y: Float, _ z: Float) -> SCNVector3
@discardableResult
func SCNVector4Make(_ x: Float, _ y: Float, _ z: Float, _ w: Float) -> SCNVector4
@discardableResult
func SCNMatrix4MakeTranslation(_ x: Float, _ y: Float, _ z: Float) -> SCNMatrix4
@discardableResult
func SCNMatrix4MakeScale(_ sx: Float, _ sy: Float, _ sz: Float) -> SCNMatrix4
@discardableResult
func SCNMatrix4Translate(_ mat: SCNMatrix4, _ x: Float, _ y: Float, _ z: Float) -> SCNMatrix4
@available(tvOS 8.0, *)
@discardableResult
func SCNMatrix4MakeRotation(_ angle: Float, _ x: Float, _ y: Float, _ z: Float) -> SCNMatrix4
@available(tvOS 8.0, *)
@discardableResult
func SCNMatrix4Scale(_ mat: SCNMatrix4, _ x: Float, _ y: Float, _ z: Float) -> SCNMatrix4
@available(tvOS 8.0, *)
@discardableResult
func SCNMatrix4Rotate(_ mat: SCNMatrix4, _ angle: Float, _ x: Float, _ y: Float, _ z: Float) -> SCNMatrix4
@available(tvOS 8.0, *)
@discardableResult
func SCNMatrix4Invert(_ mat: SCNMatrix4) -> SCNMatrix4
@available(tvOS 8.0, *)
@discardableResult
func SCNMatrix4Mult(_ matA: SCNMatrix4, _ matB: SCNMatrix4) -> SCNMatrix4
@available(tvOS 8.0, *)
@discardableResult
func SCNMatrix4IsIdentity(_ mat: SCNMatrix4) -> Bool
@available(tvOS 8.0, *)
@discardableResult
func SCNMatrix4EqualToMatrix4(_ matA: SCNMatrix4, _ matB: SCNMatrix4) -> Bool
@discardableResult
func SCNVector3FromGLKVector3(_ vector: GLKVector3) -> SCNVector3
@discardableResult
func SCNVector3ToGLKVector3(_ vector: SCNVector3) -> GLKVector3
@discardableResult
func SCNVector4FromGLKVector4(_ vector: GLKVector4) -> SCNVector4
@discardableResult
func SCNVector4ToGLKVector4(_ vector: SCNVector4) -> GLKVector4
@available(tvOS 8.0, *)
@discardableResult
func SCNMatrix4ToGLKMatrix4(_ mat: SCNMatrix4) -> GLKMatrix4
@available(tvOS 8.0, *)
@discardableResult
func SCNMatrix4FromGLKMatrix4(_ mat: GLKMatrix4) -> SCNMatrix4
extension NSValue {
  /*not inherited*/ init(scnVector3 v: SCNVector3)
  /*not inherited*/ init(scnVector4 v: SCNVector4)
  @available(tvOS 8.0, *)
  /*not inherited*/ init(scnMatrix4 v: SCNMatrix4)
  var scnVector3Value: SCNVector3 { get }
  var scnVector4Value: SCNVector4 { get }
  @available(tvOS 8.0, *)
  var scnMatrix4Value: SCNMatrix4 { get }
}
let SCNErrorDomain: String
var SCNProgramCompilationError: Int { get }
