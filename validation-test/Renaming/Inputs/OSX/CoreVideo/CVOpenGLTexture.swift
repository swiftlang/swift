
typealias CVOpenGLTexture = CVImageBuffer
@available(OSX 10.4, *)
@discardableResult
func CVOpenGLTextureGetTypeID() -> CFTypeID
@available(OSX 10.4, *)
@discardableResult
func CVOpenGLTextureGetTarget(_ image: CVOpenGLTexture) -> GLenum
@available(OSX 10.4, *)
@discardableResult
func CVOpenGLTextureGetName(_ image: CVOpenGLTexture) -> GLuint
@available(OSX 10.4, *)
@discardableResult
func CVOpenGLTextureIsFlipped(_ image: CVOpenGLTexture) -> Bool
@available(OSX 10.4, *)
func CVOpenGLTextureGetCleanTexCoords(_ image: CVOpenGLTexture, _ lowerLeft: UnsafeMutablePointer<GLfloat>!, _ lowerRight: UnsafeMutablePointer<GLfloat>!, _ upperRight: UnsafeMutablePointer<GLfloat>!, _ upperLeft: UnsafeMutablePointer<GLfloat>!)
