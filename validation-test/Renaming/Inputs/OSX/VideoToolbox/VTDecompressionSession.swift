
class VTDecompressionSession {
}
typealias VTDecompressionOutputCallback = @convention(c) (UnsafeMutablePointer<Void>?, UnsafeMutablePointer<Void>?, OSStatus, VTDecodeInfoFlags, CVImageBuffer?, CMTime, CMTime) -> Void
struct VTDecompressionOutputCallbackRecord {
  var decompressionOutputCallback: VTDecompressionOutputCallback?
  var decompressionOutputRefCon: UnsafeMutablePointer<Void>?
  init()
  init(decompressionOutputCallback decompressionOutputCallback: VTDecompressionOutputCallback?, decompressionOutputRefCon decompressionOutputRefCon: UnsafeMutablePointer<Void>?)
}
@available(OSX 10.8, *)
@discardableResult
func VTDecompressionSessionCreate(_ allocator: CFAllocator?, _ videoFormatDescription: CMVideoFormatDescription, _ videoDecoderSpecification: CFDictionary?, _ destinationImageBufferAttributes: CFDictionary?, _ outputCallback: UnsafePointer<VTDecompressionOutputCallbackRecord>?, _ decompressionSessionOut: UnsafeMutablePointer<VTDecompressionSession?>) -> OSStatus
@available(OSX 10.8, *)
func VTDecompressionSessionInvalidate(_ session: VTDecompressionSession)
@available(OSX 10.8, *)
@discardableResult
func VTDecompressionSessionGetTypeID() -> CFTypeID
@available(OSX 10.8, *)
@discardableResult
func VTDecompressionSessionDecodeFrame(_ session: VTDecompressionSession, _ sampleBuffer: CMSampleBuffer, _ decodeFlags: VTDecodeFrameFlags, _ sourceFrameRefCon: UnsafeMutablePointer<Void>?, _ infoFlagsOut: UnsafeMutablePointer<VTDecodeInfoFlags>?) -> OSStatus
typealias VTDecompressionOutputHandler = (OSStatus, VTDecodeInfoFlags, CVImageBuffer?, CMTime, CMTime) -> Void
@available(OSX 10.11, *)
@discardableResult
func VTDecompressionSessionDecodeFrameWithOutputHandler(_ session: VTDecompressionSession, _ sampleBuffer: CMSampleBuffer, _ decodeFlags: VTDecodeFrameFlags, _ infoFlagsOut: UnsafeMutablePointer<VTDecodeInfoFlags>?, _ outputHandler: VTDecompressionOutputHandler) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func VTDecompressionSessionFinishDelayedFrames(_ session: VTDecompressionSession) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func VTDecompressionSessionCanAcceptFormatDescription(_ session: VTDecompressionSession, _ newFormatDesc: CMFormatDescription) -> Bool
@available(OSX 10.8, *)
@discardableResult
func VTDecompressionSessionWaitForAsynchronousFrames(_ session: VTDecompressionSession) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func VTDecompressionSessionCopyBlackPixelBuffer(_ session: VTDecompressionSession, _ pixelBufferOut: UnsafeMutablePointer<CVPixelBuffer?>) -> OSStatus
