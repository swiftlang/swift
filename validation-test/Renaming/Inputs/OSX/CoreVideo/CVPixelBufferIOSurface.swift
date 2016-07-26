
@available(OSX 10.6, *)
let kCVPixelBufferIOSurfaceOpenGLTextureCompatibilityKey: CFString
@available(OSX 10.6, *)
let kCVPixelBufferIOSurfaceOpenGLFBOCompatibilityKey: CFString
@available(OSX 10.6, *)
let kCVPixelBufferIOSurfaceCoreAnimationCompatibilityKey: CFString
@available(OSX 10.6, *)
@discardableResult
func CVPixelBufferGetIOSurface(_ pixelBuffer: CVPixelBuffer?) -> Unmanaged<IOSurface>?
@available(OSX 10.6, *)
@discardableResult
func CVPixelBufferCreateWithIOSurface(_ allocator: CFAllocator?, _ surface: IOSurface, _ pixelBufferAttributes: CFDictionary?, _ pixelBufferOut: UnsafeMutablePointer<Unmanaged<CVPixelBuffer>?>) -> CVReturn
