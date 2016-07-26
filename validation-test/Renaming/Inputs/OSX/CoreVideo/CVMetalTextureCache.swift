
@available(OSX 10.11, *)
let kCVMetalTextureCacheMaximumTextureAgeKey: CFString
class CVMetalTextureCache {
}
@available(OSX 10.11, *)
@discardableResult
func CVMetalTextureCacheGetTypeID() -> CFTypeID
@available(OSX 10.11, *)
@discardableResult
func CVMetalTextureCacheCreate(_ allocator: CFAllocator?, _ cacheAttributes: CFDictionary?, _ metalDevice: MTLDevice, _ textureAttributes: CFDictionary?, _ cacheOut: UnsafeMutablePointer<Unmanaged<CVMetalTextureCache>?>) -> CVReturn
@available(OSX 10.11, *)
@discardableResult
func CVMetalTextureCacheCreateTextureFromImage(_ allocator: CFAllocator?, _ textureCache: CVMetalTextureCache, _ sourceImage: CVImageBuffer, _ textureAttributes: CFDictionary?, _ pixelFormat: MTLPixelFormat, _ width: Int, _ height: Int, _ planeIndex: Int, _ textureOut: UnsafeMutablePointer<Unmanaged<CVMetalTexture>?>) -> CVReturn
@available(OSX 10.11, *)
func CVMetalTextureCacheFlush(_ textureCache: CVMetalTextureCache, _ options: CVOptionFlags)
