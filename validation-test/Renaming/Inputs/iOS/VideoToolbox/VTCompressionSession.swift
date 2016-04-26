
class VTCompressionSession {
}
typealias VTCompressionOutputCallback = @convention(c) (UnsafeMutablePointer<Void>?, UnsafeMutablePointer<Void>?, OSStatus, VTEncodeInfoFlags, CMSampleBuffer?) -> Void
@available(iOS 8.0, *)
let kVTVideoEncoderSpecification_EncoderID: CFString
@available(iOS 8.0, *)
@discardableResult
func VTCompressionSessionCreate(_ allocator: CFAllocator?, _ width: Int32, _ height: Int32, _ codecType: CMVideoCodecType, _ encoderSpecification: CFDictionary?, _ sourceImageBufferAttributes: CFDictionary?, _ compressedDataAllocator: CFAllocator?, _ outputCallback: VTCompressionOutputCallback?, _ outputCallbackRefCon: UnsafeMutablePointer<Void>?, _ compressionSessionOut: UnsafeMutablePointer<VTCompressionSession?>) -> OSStatus
@available(iOS 8.0, *)
func VTCompressionSessionInvalidate(_ session: VTCompressionSession)
@available(iOS 8.0, *)
@discardableResult
func VTCompressionSessionGetTypeID() -> CFTypeID
@available(iOS 8.0, *)
@discardableResult
func VTCompressionSessionGetPixelBufferPool(_ session: VTCompressionSession) -> CVPixelBufferPool?
@available(iOS 8.0, *)
@discardableResult
func VTCompressionSessionPrepareToEncodeFrames(_ session: VTCompressionSession) -> OSStatus
@available(iOS 8.0, *)
@discardableResult
func VTCompressionSessionEncodeFrame(_ session: VTCompressionSession, _ imageBuffer: CVImageBuffer, _ presentationTimeStamp: CMTime, _ duration: CMTime, _ frameProperties: CFDictionary?, _ sourceFrameRefCon: UnsafeMutablePointer<Void>?, _ infoFlagsOut: UnsafeMutablePointer<VTEncodeInfoFlags>?) -> OSStatus
typealias VTCompressionOutputHandler = (OSStatus, VTEncodeInfoFlags, CMSampleBuffer?) -> Void
@available(iOS 9.0, *)
@discardableResult
func VTCompressionSessionEncodeFrameWithOutputHandler(_ session: VTCompressionSession, _ imageBuffer: CVImageBuffer, _ presentationTimeStamp: CMTime, _ duration: CMTime, _ frameProperties: CFDictionary?, _ infoFlagsOut: UnsafeMutablePointer<VTEncodeInfoFlags>?, _ outputHandler: VTCompressionOutputHandler) -> OSStatus
@available(iOS 8.0, *)
@discardableResult
func VTCompressionSessionCompleteFrames(_ session: VTCompressionSession, _ completeUntilPresentationTimeStamp: CMTime) -> OSStatus
struct VTCompressionSessionOptionFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var beginFinalPass: VTCompressionSessionOptionFlags { get }
}
@available(iOS 8.0, *)
@discardableResult
func VTCompressionSessionBeginPass(_ session: VTCompressionSession, _ beginPassFlags: VTCompressionSessionOptionFlags, _ reserved: UnsafeMutablePointer<UInt32>?) -> OSStatus
@available(iOS 8.0, *)
@discardableResult
func VTCompressionSessionEndPass(_ session: VTCompressionSession, _ furtherPassesRequestedOut: UnsafeMutablePointer<DarwinBoolean>?, _ reserved: UnsafeMutablePointer<UInt32>?) -> OSStatus
@available(iOS 8.0, *)
@discardableResult
func VTCompressionSessionGetTimeRangesForNextPass(_ session: VTCompressionSession, _ timeRangeCountOut: UnsafeMutablePointer<CMItemCount>, _ timeRangeArrayOut: UnsafeMutablePointer<UnsafePointer<CMTimeRange>?>) -> OSStatus
