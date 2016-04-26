
let GLKQuaternionIdentity: GLKQuaternion
@discardableResult
func GLKQuaternionMakeWithMatrix3(_ matrix: GLKMatrix3) -> GLKQuaternion
@discardableResult
func GLKQuaternionMakeWithMatrix4(_ matrix: GLKMatrix4) -> GLKQuaternion
@discardableResult
func GLKQuaternionAngle(_ quaternion: GLKQuaternion) -> Float
@discardableResult
func GLKQuaternionAxis(_ quaternion: GLKQuaternion) -> GLKVector3
@discardableResult
func GLKQuaternionSlerp(_ quaternionStart: GLKQuaternion, _ quaternionEnd: GLKQuaternion, _ t: Float) -> GLKQuaternion
func GLKQuaternionRotateVector3Array(_ quaternion: GLKQuaternion, _ vectors: UnsafeMutablePointer<GLKVector3>!, _ vectorCount: Int)
func GLKQuaternionRotateVector4Array(_ quaternion: GLKQuaternion, _ vectors: UnsafeMutablePointer<GLKVector4>!, _ vectorCount: Int)
@discardableResult
func GLKQuaternionMake(_ x: Float, _ y: Float, _ z: Float, _ w: Float) -> GLKQuaternion
@discardableResult
func GLKQuaternionMakeWithVector3(_ vector: GLKVector3, _ scalar: Float) -> GLKQuaternion
@discardableResult
func GLKQuaternionMakeWithArray(_ values: UnsafeMutablePointer<Float>!) -> GLKQuaternion
@discardableResult
func GLKQuaternionMakeWithAngleAndAxis(_ radians: Float, _ x: Float, _ y: Float, _ z: Float) -> GLKQuaternion
@discardableResult
func GLKQuaternionMakeWithAngleAndVector3Axis(_ radians: Float, _ axisVector: GLKVector3) -> GLKQuaternion
@discardableResult
func GLKQuaternionAdd(_ quaternionLeft: GLKQuaternion, _ quaternionRight: GLKQuaternion) -> GLKQuaternion
@discardableResult
func GLKQuaternionSubtract(_ quaternionLeft: GLKQuaternion, _ quaternionRight: GLKQuaternion) -> GLKQuaternion
@discardableResult
func GLKQuaternionMultiply(_ quaternionLeft: GLKQuaternion, _ quaternionRight: GLKQuaternion) -> GLKQuaternion
@discardableResult
func GLKQuaternionLength(_ quaternion: GLKQuaternion) -> Float
@discardableResult
func GLKQuaternionConjugate(_ quaternion: GLKQuaternion) -> GLKQuaternion
@discardableResult
func GLKQuaternionInvert(_ quaternion: GLKQuaternion) -> GLKQuaternion
@discardableResult
func GLKQuaternionNormalize(_ quaternion: GLKQuaternion) -> GLKQuaternion
@discardableResult
func GLKQuaternionRotateVector3(_ quaternion: GLKQuaternion, _ vector: GLKVector3) -> GLKVector3
@discardableResult
func GLKQuaternionRotateVector4(_ quaternion: GLKQuaternion, _ vector: GLKVector4) -> GLKVector4
