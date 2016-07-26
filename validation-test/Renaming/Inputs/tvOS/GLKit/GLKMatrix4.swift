
let GLKMatrix4Identity: GLKMatrix4
@discardableResult
func GLKMatrix4Invert(_ matrix: GLKMatrix4, _ isInvertible: UnsafeMutablePointer<Bool>?) -> GLKMatrix4
@discardableResult
func GLKMatrix4InvertAndTranspose(_ matrix: GLKMatrix4, _ isInvertible: UnsafeMutablePointer<Bool>?) -> GLKMatrix4
@discardableResult
func GLKMatrix4Make(_ m00: Float, _ m01: Float, _ m02: Float, _ m03: Float, _ m10: Float, _ m11: Float, _ m12: Float, _ m13: Float, _ m20: Float, _ m21: Float, _ m22: Float, _ m23: Float, _ m30: Float, _ m31: Float, _ m32: Float, _ m33: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4MakeAndTranspose(_ m00: Float, _ m01: Float, _ m02: Float, _ m03: Float, _ m10: Float, _ m11: Float, _ m12: Float, _ m13: Float, _ m20: Float, _ m21: Float, _ m22: Float, _ m23: Float, _ m30: Float, _ m31: Float, _ m32: Float, _ m33: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4MakeWithArray(_ values: UnsafeMutablePointer<Float>!) -> GLKMatrix4
@discardableResult
func GLKMatrix4MakeWithArrayAndTranspose(_ values: UnsafeMutablePointer<Float>!) -> GLKMatrix4
func __builtin_neon_vld4q_v(_ _: UnsafeMutablePointer<Void>!, _ _: UnsafePointer<Void>!, _ _: Int32)
@discardableResult
func GLKMatrix4MakeWithRows(_ row0: GLKVector4, _ row1: GLKVector4, _ row2: GLKVector4, _ row3: GLKVector4) -> GLKMatrix4
@discardableResult
func GLKMatrix4MakeWithColumns(_ column0: GLKVector4, _ column1: GLKVector4, _ column2: GLKVector4, _ column3: GLKVector4) -> GLKMatrix4
@discardableResult
func GLKMatrix4MakeWithQuaternion(_ quaternion: GLKQuaternion) -> GLKMatrix4
@discardableResult
func GLKMatrix4MakeTranslation(_ tx: Float, _ ty: Float, _ tz: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4MakeScale(_ sx: Float, _ sy: Float, _ sz: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4MakeRotation(_ radians: Float, _ x: Float, _ y: Float, _ z: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4MakeXRotation(_ radians: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4MakeYRotation(_ radians: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4MakeZRotation(_ radians: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4MakePerspective(_ fovyRadians: Float, _ aspect: Float, _ nearZ: Float, _ farZ: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4MakeFrustum(_ left: Float, _ right: Float, _ bottom: Float, _ top: Float, _ nearZ: Float, _ farZ: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4MakeOrtho(_ left: Float, _ right: Float, _ bottom: Float, _ top: Float, _ nearZ: Float, _ farZ: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4MakeLookAt(_ eyeX: Float, _ eyeY: Float, _ eyeZ: Float, _ centerX: Float, _ centerY: Float, _ centerZ: Float, _ upX: Float, _ upY: Float, _ upZ: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4GetMatrix3(_ matrix: GLKMatrix4) -> GLKMatrix3
@discardableResult
func GLKMatrix4GetMatrix2(_ matrix: GLKMatrix4) -> GLKMatrix2
@discardableResult
func GLKMatrix4GetRow(_ matrix: GLKMatrix4, _ row: Int32) -> GLKVector4
@discardableResult
func GLKMatrix4GetColumn(_ matrix: GLKMatrix4, _ column: Int32) -> GLKVector4
@discardableResult
func GLKMatrix4SetRow(_ matrix: GLKMatrix4, _ row: Int32, _ vector: GLKVector4) -> GLKMatrix4
@discardableResult
func GLKMatrix4SetColumn(_ matrix: GLKMatrix4, _ column: Int32, _ vector: GLKVector4) -> GLKMatrix4
@discardableResult
func GLKMatrix4Transpose(_ matrix: GLKMatrix4) -> GLKMatrix4
@discardableResult
func GLKMatrix4Multiply(_ matrixLeft: GLKMatrix4, _ matrixRight: GLKMatrix4) -> GLKMatrix4
@discardableResult
func GLKMatrix4Add(_ matrixLeft: GLKMatrix4, _ matrixRight: GLKMatrix4) -> GLKMatrix4
@discardableResult
func GLKMatrix4Subtract(_ matrixLeft: GLKMatrix4, _ matrixRight: GLKMatrix4) -> GLKMatrix4
@discardableResult
func GLKMatrix4Translate(_ matrix: GLKMatrix4, _ tx: Float, _ ty: Float, _ tz: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4TranslateWithVector3(_ matrix: GLKMatrix4, _ translationVector: GLKVector3) -> GLKMatrix4
@discardableResult
func GLKMatrix4TranslateWithVector4(_ matrix: GLKMatrix4, _ translationVector: GLKVector4) -> GLKMatrix4
@discardableResult
func GLKMatrix4Scale(_ matrix: GLKMatrix4, _ sx: Float, _ sy: Float, _ sz: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4ScaleWithVector3(_ matrix: GLKMatrix4, _ scaleVector: GLKVector3) -> GLKMatrix4
@discardableResult
func GLKMatrix4ScaleWithVector4(_ matrix: GLKMatrix4, _ scaleVector: GLKVector4) -> GLKMatrix4
@discardableResult
func GLKMatrix4Rotate(_ matrix: GLKMatrix4, _ radians: Float, _ x: Float, _ y: Float, _ z: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4RotateWithVector3(_ matrix: GLKMatrix4, _ radians: Float, _ axisVector: GLKVector3) -> GLKMatrix4
@discardableResult
func GLKMatrix4RotateWithVector4(_ matrix: GLKMatrix4, _ radians: Float, _ axisVector: GLKVector4) -> GLKMatrix4
@discardableResult
func GLKMatrix4RotateX(_ matrix: GLKMatrix4, _ radians: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4RotateY(_ matrix: GLKMatrix4, _ radians: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4RotateZ(_ matrix: GLKMatrix4, _ radians: Float) -> GLKMatrix4
@discardableResult
func GLKMatrix4MultiplyVector3(_ matrixLeft: GLKMatrix4, _ vectorRight: GLKVector3) -> GLKVector3
@discardableResult
func GLKMatrix4MultiplyVector3WithTranslation(_ matrixLeft: GLKMatrix4, _ vectorRight: GLKVector3) -> GLKVector3
@discardableResult
func GLKMatrix4MultiplyAndProjectVector3(_ matrixLeft: GLKMatrix4, _ vectorRight: GLKVector3) -> GLKVector3
func GLKMatrix4MultiplyVector3Array(_ matrix: GLKMatrix4, _ vectors: UnsafeMutablePointer<GLKVector3>, _ vectorCount: Int)
func GLKMatrix4MultiplyVector3ArrayWithTranslation(_ matrix: GLKMatrix4, _ vectors: UnsafeMutablePointer<GLKVector3>, _ vectorCount: Int)
func GLKMatrix4MultiplyAndProjectVector3Array(_ matrix: GLKMatrix4, _ vectors: UnsafeMutablePointer<GLKVector3>, _ vectorCount: Int)
@discardableResult
func GLKMatrix4MultiplyVector4(_ matrixLeft: GLKMatrix4, _ vectorRight: GLKVector4) -> GLKVector4
func GLKMatrix4MultiplyVector4Array(_ matrix: GLKMatrix4, _ vectors: UnsafeMutablePointer<GLKVector4>, _ vectorCount: Int)
