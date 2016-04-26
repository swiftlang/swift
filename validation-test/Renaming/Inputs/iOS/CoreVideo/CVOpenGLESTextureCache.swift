
class CVOpenGLESTextureCache {
}
var COREVIDEO_USE_EAGLCONTEXT_CLASS_IN_API: Int32 { get }
typealias CVEAGLContext = EAGLContext
@available(iOS 5.0, *)
let kCVOpenGLESTextureCacheMaximumTextureAgeKey: CFString
@available(iOS 5.0, *)
@discardableResult
func CVOpenGLESTextureCacheGetTypeID() -> CFTypeID
@available(iOS 5.0, *)
@discardableResult
func CVOpenGLESTextureCacheCreate(_ allocator: CFAllocator?, _ cacheAttributes: CFDictionary?, _ eaglContext: CVEAGLContext, _ textureAttributes: CFDictionary?, _ cacheOut: UnsafeMutablePointer<CVOpenGLESTextureCache?>) -> CVReturn
@available(iOS 5.0, *)
@discardableResult
func CVOpenGLESTextureCacheCreateTextureFromImage(_ allocator: CFAllocator?, _ textureCache: CVOpenGLESTextureCache, _ sourceImage: CVImageBuffer, _ textureAttributes: CFDictionary?, _ target: GLenum, _ internalFormat: GLint, _ width: GLsizei, _ height: GLsizei, _ format: GLenum, _ type: GLenum, _ planeIndex: Int, _ textureOut: UnsafeMutablePointer<CVOpenGLESTexture?>) -> CVReturn
@available(iOS 5.0, *)
func CVOpenGLESTextureCacheFlush(_ textureCache: CVOpenGLESTextureCache, _ options: CVOptionFlags)
