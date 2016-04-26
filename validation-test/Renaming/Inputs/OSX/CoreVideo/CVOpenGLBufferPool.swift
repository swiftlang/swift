
class CVOpenGLBufferPool {
}
@available(OSX 10.4, *)
let kCVOpenGLBufferPoolMinimumBufferCountKey: CFString
@available(OSX 10.4, *)
let kCVOpenGLBufferPoolMaximumBufferAgeKey: CFString
@available(OSX 10.4, *)
@discardableResult
func CVOpenGLBufferPoolGetTypeID() -> CFTypeID
@available(OSX 10.4, *)
@discardableResult
func CVOpenGLBufferPoolCreate(_ allocator: CFAllocator?, _ poolAttributes: CFDictionary?, _ openGLBufferAttributes: CFDictionary?, _ poolOut: UnsafeMutablePointer<CVOpenGLBufferPool?>) -> CVReturn
@available(OSX 10.4, *)
@discardableResult
func CVOpenGLBufferPoolGetAttributes(_ pool: CVOpenGLBufferPool) -> Unmanaged<CFDictionary>?
@available(OSX 10.4, *)
@discardableResult
func CVOpenGLBufferPoolGetOpenGLBufferAttributes(_ pool: CVOpenGLBufferPool) -> Unmanaged<CFDictionary>?
@available(OSX 10.4, *)
@discardableResult
func CVOpenGLBufferPoolCreateOpenGLBuffer(_ allocator: CFAllocator?, _ openGLBufferPool: CVOpenGLBufferPool, _ openGLBufferOut: UnsafeMutablePointer<CVOpenGLBuffer?>) -> CVReturn
