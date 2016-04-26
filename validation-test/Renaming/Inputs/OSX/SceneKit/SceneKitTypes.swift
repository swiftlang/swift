
typealias SCNMatrix4 = CATransform3D
struct SCNVector3 {
  var x: CGFloat
  var y: CGFloat
  var z: CGFloat
  init()
  init(x x: CGFloat, y y: CGFloat, z z: CGFloat)
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
  var x: CGFloat
  var y: CGFloat
  var z: CGFloat
  var w: CGFloat
  init()
  init(x x: CGFloat, y y: CGFloat, z z: CGFloat, w w: CGFloat)
}

extension SCNVector4 {
  init(_ x: Float, _ y: Float, _ z: Float, _ w: Float)
  init(_ x: CGFloat, _ y: CGFloat, _ z: CGFloat, _ w: CGFloat)
  init(_ x: Double, _ y: Double, _ z: Double, _ w: Double)
  init(_ x: Int, _ y: Int, _ z: Int, _ w: Int)
  init(_ v: float4)
  init(_ v: double4)
}
typealias SCNQuaternion = SCNVector4
@available(OSX 10.10, *)
let SCNMatrix4Identity: SCNMatrix4
@available(OSX 10.10, *)
let SCNVector3Zero: SCNVector3
@available(OSX 10.10, *)
let SCNVector4Zero: SCNVector4
@discardableResult
func SCNVector3EqualToVector3(_ a: SCNVector3, _ b: SCNVector3) -> Bool
@discardableResult
func SCNVector4EqualToVector4(_ a: SCNVector4, _ b: SCNVector4) -> Bool
@discardableResult
func SCNVector3Make(_ x: CGFloat, _ y: CGFloat, _ z: CGFloat) -> SCNVector3
@discardableResult
func SCNVector4Make(_ x: CGFloat, _ y: CGFloat, _ z: CGFloat, _ w: CGFloat) -> SCNVector4
@discardableResult
func SCNMatrix4MakeTranslation(_ x: CGFloat, _ y: CGFloat, _ z: CGFloat) -> SCNMatrix4
@discardableResult
func SCNMatrix4MakeScale(_ sx: CGFloat, _ sy: CGFloat, _ sz: CGFloat) -> SCNMatrix4
@discardableResult
func SCNMatrix4Translate(_ mat: SCNMatrix4, _ x: CGFloat, _ y: CGFloat, _ z: CGFloat) -> SCNMatrix4
@available(OSX 10.10, *)
@discardableResult
func SCNMatrix4MakeRotation(_ angle: CGFloat, _ x: CGFloat, _ y: CGFloat, _ z: CGFloat) -> SCNMatrix4
@available(OSX 10.10, *)
@discardableResult
func SCNMatrix4Scale(_ mat: SCNMatrix4, _ x: CGFloat, _ y: CGFloat, _ z: CGFloat) -> SCNMatrix4
@available(OSX 10.10, *)
@discardableResult
func SCNMatrix4Rotate(_ mat: SCNMatrix4, _ angle: CGFloat, _ x: CGFloat, _ y: CGFloat, _ z: CGFloat) -> SCNMatrix4
@available(OSX 10.10, *)
@discardableResult
func SCNMatrix4Invert(_ mat: SCNMatrix4) -> SCNMatrix4
@available(OSX 10.10, *)
@discardableResult
func SCNMatrix4Mult(_ matA: SCNMatrix4, _ matB: SCNMatrix4) -> SCNMatrix4
@available(OSX 10.10, *)
@discardableResult
func SCNMatrix4IsIdentity(_ mat: SCNMatrix4) -> Bool
@available(OSX 10.10, *)
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
@available(OSX 10.10, *)
@discardableResult
func SCNMatrix4ToGLKMatrix4(_ mat: SCNMatrix4) -> GLKMatrix4
@available(OSX 10.10, *)
@discardableResult
func SCNMatrix4FromGLKMatrix4(_ mat: GLKMatrix4) -> SCNMatrix4
extension NSValue {
  /*not inherited*/ init(scnVector3 v: SCNVector3)
  /*not inherited*/ init(scnVector4 v: SCNVector4)
  @available(OSX 10.10, *)
  /*not inherited*/ init(scnMatrix4 v: SCNMatrix4)
  var scnVector3Value: SCNVector3 { get }
  var scnVector4Value: SCNVector4 { get }
  @available(OSX 10.10, *)
  var scnMatrix4Value: SCNMatrix4 { get }
}
let SCNErrorDomain: String
var SCNProgramCompilationError: Int { get }
