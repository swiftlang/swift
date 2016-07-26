
class VTCompressionSession {
}
typealias VTCompressionOutputCallback = @convention(c) (UnsafeMutablePointer<Void>?, UnsafeMutablePointer<Void>?, OSStatus, VTEncodeInfoFlags, CMSampleBuffer?) -> Void
@available(OSX 10.8, *)
let kVTVideoEncoderSpecification_EncoderID: CFString
@available(OSX 10.8, *)
@discardableResult
func VTCompressionSessionCreate(_ allocator: CFAllocator?, _ width: Int32, _ height: Int32, _ codecType: CMVideoCodecType, _ encoderSpecification: CFDictionary?, _ sourceImageBufferAttributes: CFDictionary?, _ compressedDataAllocator: CFAllocator?, _ outputCallback: VTCompressionOutputCallback?, _ outputCallbackRefCon: UnsafeMutablePointer<Void>?, _ compressionSessionOut: UnsafeMutablePointer<VTCompressionSession?>) -> OSStatus
@available(OSX 10.8, *)
func VTCompressionSessionInvalidate(_ session: VTCompressionSession)
@available(OSX 10.8, *)
@discardableResult
func VTCompressionSessionGetTypeID() -> CFTypeID
@available(OSX 10.8, *)
@discardableResult
func VTCompressionSessionGetPixelBufferPool(_ session: VTCompressionSession) -> CVPixelBufferPool?
@available(OSX 10.9, *)
@discardableResult
func VTCompressionSessionPrepareToEncodeFrames(_ session: VTCompressionSession) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func VTCompressionSessionEncodeFrame(_ session: VTCompressionSession, _ imageBuffer: CVImageBuffer, _ presentationTimeStamp: CMTime, _ duration: CMTime, _ frameProperties: CFDictionary?, _ sourceFrameRefCon: UnsafeMutablePointer<Void>?, _ infoFlagsOut: UnsafeMutablePointer<VTEncodeInfoFlags>?) -> OSStatus
typealias VTCompressionOutputHandler = (OSStatus, VTEncodeInfoFlags, CMSampleBuffer?) -> Void
@available(OSX 10.11, *)
@discardableResult
func VTCompressionSessionEncodeFrameWithOutputHandler(_ session: VTCompressionSession, _ imageBuffer: CVImageBuffer, _ presentationTimeStamp: CMTime, _ duration: CMTime, _ frameProperties: CFDictionary?, _ infoFlagsOut: UnsafeMutablePointer<VTEncodeInfoFlags>?, _ outputHandler: VTCompressionOutputHandler) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func VTCompressionSessionCompleteFrames(_ session: VTCompressionSession, _ completeUntilPresentationTimeStamp: CMTime) -> OSStatus
struct VTCompressionSessionOptionFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var beginFinalPass: VTCompressionSessionOptionFlags { get }
}
@available(OSX 10.10, *)
@discardableResult
func VTCompressionSessionBeginPass(_ session: VTCompressionSession, _ beginPassFlags: VTCompressionSessionOptionFlags, _ reserved: UnsafeMutablePointer<UInt32>?) -> OSStatus
@available(OSX 10.10, *)
@discardableResult
func VTCompressionSessionEndPass(_ session: VTCompressionSession, _ furtherPassesRequestedOut: UnsafeMutablePointer<DarwinBoolean>?, _ reserved: UnsafeMutablePointer<UInt32>?) -> OSStatus
@available(OSX 10.10, *)
@discardableResult
func VTCompressionSessionGetTimeRangesForNextPass(_ session: VTCompressionSession, _ timeRangeCountOut: UnsafeMutablePointer<CMItemCount>, _ timeRangeArrayOut: UnsafeMutablePointer<UnsafePointer<CMTimeRange>?>) -> OSStatus
