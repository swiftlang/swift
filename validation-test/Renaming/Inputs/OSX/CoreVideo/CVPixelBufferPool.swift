
class CVPixelBufferPool {
}
@available(OSX 10.4, *)
let kCVPixelBufferPoolMinimumBufferCountKey: CFString
@available(OSX 10.4, *)
let kCVPixelBufferPoolMaximumBufferAgeKey: CFString
@available(OSX 10.4, *)
@discardableResult
func CVPixelBufferPoolGetTypeID() -> CFTypeID
@available(OSX 10.4, *)
@discardableResult
func CVPixelBufferPoolCreate(_ allocator: CFAllocator?, _ poolAttributes: CFDictionary?, _ pixelBufferAttributes: CFDictionary?, _ poolOut: UnsafeMutablePointer<CVPixelBufferPool?>) -> CVReturn
@available(OSX 10.4, *)
@discardableResult
func CVPixelBufferPoolGetAttributes(_ pool: CVPixelBufferPool) -> Unmanaged<CFDictionary>?
@available(OSX 10.4, *)
@discardableResult
func CVPixelBufferPoolGetPixelBufferAttributes(_ pool: CVPixelBufferPool) -> Unmanaged<CFDictionary>?
@available(OSX 10.4, *)
@discardableResult
func CVPixelBufferPoolCreatePixelBuffer(_ allocator: CFAllocator?, _ pixelBufferPool: CVPixelBufferPool, _ pixelBufferOut: UnsafeMutablePointer<CVPixelBuffer?>) -> CVReturn
@available(OSX 10.7, *)
@discardableResult
func CVPixelBufferPoolCreatePixelBufferWithAuxAttributes(_ allocator: CFAllocator?, _ pixelBufferPool: CVPixelBufferPool, _ auxAttributes: CFDictionary?, _ pixelBufferOut: UnsafeMutablePointer<CVPixelBuffer?>) -> CVReturn
@available(OSX 10.7, *)
let kCVPixelBufferPoolAllocationThresholdKey: CFString
@available(OSX 10.7, *)
let kCVPixelBufferPoolFreeBufferNotification: CFString
typealias CVPixelBufferPoolFlushFlags = CVOptionFlags
var kCVPixelBufferPoolFlushExcessBuffers: CVPixelBufferPoolFlushFlags { get }
func CVPixelBufferPoolFlush(_ pool: CVPixelBufferPool, _ options: CVPixelBufferPoolFlushFlags)
