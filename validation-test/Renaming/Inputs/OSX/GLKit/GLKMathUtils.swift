
@discardableResult
func GLKMathDegreesToRadians(_ degrees: Float) -> Float
@discardableResult
func GLKMathRadiansToDegrees(_ radians: Float) -> Float
@discardableResult
func GLKMathProject(_ object: GLKVector3, _ model: GLKMatrix4, _ projection: GLKMatrix4, _ viewport: UnsafeMutablePointer<Int32>) -> GLKVector3
@discardableResult
func GLKMathUnproject(_ window: GLKVector3, _ model: GLKMatrix4, _ projection: GLKMatrix4, _ viewport: UnsafeMutablePointer<Int32>, _ success: UnsafeMutablePointer<Bool>) -> GLKVector3
@discardableResult
func NSStringFromGLKMatrix2(_ matrix: GLKMatrix2) -> String
@discardableResult
func NSStringFromGLKMatrix3(_ matrix: GLKMatrix3) -> String
@discardableResult
func NSStringFromGLKMatrix4(_ matrix: GLKMatrix4) -> String
@discardableResult
func NSStringFromGLKVector2(_ vector: GLKVector2) -> String
@discardableResult
func NSStringFromGLKVector3(_ vector: GLKVector3) -> String
@discardableResult
func NSStringFromGLKVector4(_ vector: GLKVector4) -> String
@discardableResult
func NSStringFromGLKQuaternion(_ quaternion: GLKQuaternion) -> String
