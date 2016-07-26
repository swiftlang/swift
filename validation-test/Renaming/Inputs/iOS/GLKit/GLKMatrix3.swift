
let GLKMatrix3Identity: GLKMatrix3
@discardableResult
func GLKMatrix3Invert(_ matrix: GLKMatrix3, _ isInvertible: UnsafeMutablePointer<Bool>!) -> GLKMatrix3
@discardableResult
func GLKMatrix3InvertAndTranspose(_ matrix: GLKMatrix3, _ isInvertible: UnsafeMutablePointer<Bool>!) -> GLKMatrix3
@discardableResult
func GLKMatrix3Make(_ m00: Float, _ m01: Float, _ m02: Float, _ m10: Float, _ m11: Float, _ m12: Float, _ m20: Float, _ m21: Float, _ m22: Float) -> GLKMatrix3
@discardableResult
func GLKMatrix3MakeAndTranspose(_ m00: Float, _ m01: Float, _ m02: Float, _ m10: Float, _ m11: Float, _ m12: Float, _ m20: Float, _ m21: Float, _ m22: Float) -> GLKMatrix3
@discardableResult
func GLKMatrix3MakeWithArray(_ values: UnsafeMutablePointer<Float>!) -> GLKMatrix3
@discardableResult
func GLKMatrix3MakeWithArrayAndTranspose(_ values: UnsafeMutablePointer<Float>!) -> GLKMatrix3
@discardableResult
func GLKMatrix3MakeWithRows(_ row0: GLKVector3, _ row1: GLKVector3, _ row2: GLKVector3) -> GLKMatrix3
@discardableResult
func GLKMatrix3MakeWithColumns(_ column0: GLKVector3, _ column1: GLKVector3, _ column2: GLKVector3) -> GLKMatrix3
@discardableResult
func GLKMatrix3MakeWithQuaternion(_ quaternion: GLKQuaternion) -> GLKMatrix3
@discardableResult
func GLKMatrix3MakeScale(_ sx: Float, _ sy: Float, _ sz: Float) -> GLKMatrix3
@discardableResult
func GLKMatrix3MakeRotation(_ radians: Float, _ x: Float, _ y: Float, _ z: Float) -> GLKMatrix3
@discardableResult
func GLKMatrix3MakeXRotation(_ radians: Float) -> GLKMatrix3
@discardableResult
func GLKMatrix3MakeYRotation(_ radians: Float) -> GLKMatrix3
@discardableResult
func GLKMatrix3MakeZRotation(_ radians: Float) -> GLKMatrix3
@discardableResult
func GLKMatrix3GetMatrix2(_ matrix: GLKMatrix3) -> GLKMatrix2
@discardableResult
func GLKMatrix3GetRow(_ matrix: GLKMatrix3, _ row: Int32) -> GLKVector3
@discardableResult
func GLKMatrix3GetColumn(_ matrix: GLKMatrix3, _ column: Int32) -> GLKVector3
@discardableResult
func GLKMatrix3SetRow(_ matrix: GLKMatrix3, _ row: Int32, _ vector: GLKVector3) -> GLKMatrix3
@discardableResult
func GLKMatrix3SetColumn(_ matrix: GLKMatrix3, _ column: Int32, _ vector: GLKVector3) -> GLKMatrix3
@discardableResult
func GLKMatrix3Transpose(_ matrix: GLKMatrix3) -> GLKMatrix3
@discardableResult
func GLKMatrix3Multiply(_ matrixLeft: GLKMatrix3, _ matrixRight: GLKMatrix3) -> GLKMatrix3
@discardableResult
func __builtin_neon_vgetq_lane_f32(_ _: float4, _ _: Int32) -> Float
@discardableResult
func __builtin___memcpy_chk(_ _: UnsafeMutablePointer<Void>!, _ _: UnsafePointer<Void>!, _ _: UInt, _ _: UInt) -> UnsafeMutablePointer<Void>!
@discardableResult
func GLKMatrix3Add(_ matrixLeft: GLKMatrix3, _ matrixRight: GLKMatrix3) -> GLKMatrix3
@discardableResult
func GLKMatrix3Subtract(_ matrixLeft: GLKMatrix3, _ matrixRight: GLKMatrix3) -> GLKMatrix3
@discardableResult
func GLKMatrix3Scale(_ matrix: GLKMatrix3, _ sx: Float, _ sy: Float, _ sz: Float) -> GLKMatrix3
@discardableResult
func GLKMatrix3ScaleWithVector3(_ matrix: GLKMatrix3, _ scaleVector: GLKVector3) -> GLKMatrix3
@discardableResult
func GLKMatrix3ScaleWithVector4(_ matrix: GLKMatrix3, _ scaleVector: GLKVector4) -> GLKMatrix3
@discardableResult
func GLKMatrix3Rotate(_ matrix: GLKMatrix3, _ radians: Float, _ x: Float, _ y: Float, _ z: Float) -> GLKMatrix3
@discardableResult
func GLKMatrix3RotateWithVector3(_ matrix: GLKMatrix3, _ radians: Float, _ axisVector: GLKVector3) -> GLKMatrix3
@discardableResult
func GLKMatrix3RotateWithVector4(_ matrix: GLKMatrix3, _ radians: Float, _ axisVector: GLKVector4) -> GLKMatrix3
@discardableResult
func GLKMatrix3RotateX(_ matrix: GLKMatrix3, _ radians: Float) -> GLKMatrix3
@discardableResult
func GLKMatrix3RotateY(_ matrix: GLKMatrix3, _ radians: Float) -> GLKMatrix3
@discardableResult
func GLKMatrix3RotateZ(_ matrix: GLKMatrix3, _ radians: Float) -> GLKMatrix3
@discardableResult
func GLKMatrix3MultiplyVector3(_ matrixLeft: GLKMatrix3, _ vectorRight: GLKVector3) -> GLKVector3
func GLKMatrix3MultiplyVector3Array(_ matrix: GLKMatrix3, _ vectors: UnsafeMutablePointer<GLKVector3>, _ vectorCount: Int)
