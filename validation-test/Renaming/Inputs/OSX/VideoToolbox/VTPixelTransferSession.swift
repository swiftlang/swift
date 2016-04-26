
class VTPixelTransferSession {
}
@available(OSX 10.8, *)
@discardableResult
func VTPixelTransferSessionCreate(_ allocator: CFAllocator?, _ pixelTransferSessionOut: UnsafeMutablePointer<VTPixelTransferSession?>) -> OSStatus
@available(OSX 10.8, *)
func VTPixelTransferSessionInvalidate(_ session: VTPixelTransferSession)
@available(OSX 10.8, *)
@discardableResult
func VTPixelTransferSessionGetTypeID() -> CFTypeID
@available(OSX 10.8, *)
@discardableResult
func VTPixelTransferSessionTransferImage(_ session: VTPixelTransferSession, _ sourceBuffer: CVPixelBuffer, _ destinationBuffer: CVPixelBuffer) -> OSStatus
