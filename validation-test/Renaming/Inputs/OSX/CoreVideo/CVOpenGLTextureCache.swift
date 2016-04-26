
class CVOpenGLTextureCache {
}
@available(OSX 10.4, *)
let kCVOpenGLTextureCacheChromaSamplingModeKey: CFString
@available(OSX 10.4, *)
let kCVOpenGLTextureCacheChromaSamplingModeAutomatic: CFString
@available(OSX 10.4, *)
let kCVOpenGLTextureCacheChromaSamplingModeHighestQuality: CFString
@available(OSX 10.4, *)
let kCVOpenGLTextureCacheChromaSamplingModeBestPerformance: CFString
@available(OSX 10.4, *)
@discardableResult
func CVOpenGLTextureCacheGetTypeID() -> CFTypeID
@available(OSX 10.4, *)
@discardableResult
func CVOpenGLTextureCacheCreate(_ allocator: CFAllocator?, _ cacheAttributes: CFDictionary?, _ cglContext: CGLContextObj, _ cglPixelFormat: CGLPixelFormatObj, _ textureAttributes: CFDictionary?, _ cacheOut: UnsafeMutablePointer<CVOpenGLTextureCache?>) -> CVReturn
@available(OSX 10.4, *)
@discardableResult
func CVOpenGLTextureCacheCreateTextureFromImage(_ allocator: CFAllocator?, _ textureCache: CVOpenGLTextureCache, _ sourceImage: CVImageBuffer, _ attributes: CFDictionary?, _ textureOut: UnsafeMutablePointer<CVOpenGLTexture?>) -> CVReturn
@available(OSX 10.4, *)
func CVOpenGLTextureCacheFlush(_ textureCache: CVOpenGLTextureCache, _ options: CVOptionFlags)
