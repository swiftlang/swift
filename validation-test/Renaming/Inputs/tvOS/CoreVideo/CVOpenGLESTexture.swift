
typealias CVOpenGLESTexture = CVImageBuffer
@available(tvOS 5.0, *)
@discardableResult
func CVOpenGLESTextureGetTypeID() -> CFTypeID
@available(tvOS 5.0, *)
@discardableResult
func CVOpenGLESTextureGetTarget(_ image: CVOpenGLESTexture) -> GLenum
@available(tvOS 5.0, *)
@discardableResult
func CVOpenGLESTextureGetName(_ image: CVOpenGLESTexture) -> GLuint
@available(tvOS 5.0, *)
@discardableResult
func CVOpenGLESTextureIsFlipped(_ image: CVOpenGLESTexture) -> Bool
@available(tvOS 5.0, *)
func CVOpenGLESTextureGetCleanTexCoords(_ image: CVOpenGLESTexture, _ lowerLeft: UnsafeMutablePointer<GLfloat>!, _ lowerRight: UnsafeMutablePointer<GLfloat>!, _ upperRight: UnsafeMutablePointer<GLfloat>!, _ upperLeft: UnsafeMutablePointer<GLfloat>!)
