
class MTAudioProcessingTap {
}
@available(iOS 6.0, *)
@discardableResult
func MTAudioProcessingTapGetTypeID() -> CFTypeID
typealias MTAudioProcessingTapCreationFlags = UInt32
var kMTAudioProcessingTapCreationFlag_PreEffects: MTAudioProcessingTapCreationFlags { get }
var kMTAudioProcessingTapCreationFlag_PostEffects: MTAudioProcessingTapCreationFlags { get }
typealias MTAudioProcessingTapFlags = UInt32
var kMTAudioProcessingTapFlag_StartOfStream: MTAudioProcessingTapFlags { get }
var kMTAudioProcessingTapFlag_EndOfStream: MTAudioProcessingTapFlags { get }
typealias MTAudioProcessingTapInitCallback = @convention(c) (MTAudioProcessingTap, UnsafeMutablePointer<Void>?, UnsafeMutablePointer<UnsafeMutablePointer<Void>?>) -> Void
typealias MTAudioProcessingTapFinalizeCallback = @convention(c) (MTAudioProcessingTap) -> Void
typealias MTAudioProcessingTapPrepareCallback = @convention(c) (MTAudioProcessingTap, CMItemCount, UnsafePointer<AudioStreamBasicDescription>) -> Void
typealias MTAudioProcessingTapUnprepareCallback = @convention(c) (MTAudioProcessingTap) -> Void
typealias MTAudioProcessingTapProcessCallback = @convention(c) (MTAudioProcessingTap, CMItemCount, MTAudioProcessingTapFlags, UnsafeMutablePointer<AudioBufferList>, UnsafeMutablePointer<CMItemCount>, UnsafeMutablePointer<MTAudioProcessingTapFlags>) -> Void
var kMTAudioProcessingTapCallbacksVersion_0: Int32 { get }
struct MTAudioProcessingTapCallbacks {
  var version: Int32
  var clientInfo: UnsafeMutablePointer<Void>?
  var `init`: MTAudioProcessingTapInitCallback?
  var finalize: MTAudioProcessingTapFinalizeCallback?
  var prepare: MTAudioProcessingTapPrepareCallback?
  var unprepare: MTAudioProcessingTapUnprepareCallback?
  var process: MTAudioProcessingTapProcessCallback
}
@available(iOS 6.0, *)
@discardableResult
func MTAudioProcessingTapCreate(_ allocator: CFAllocator?, _ callbacks: UnsafePointer<MTAudioProcessingTapCallbacks>, _ flags: MTAudioProcessingTapCreationFlags, _ tapOut: UnsafeMutablePointer<Unmanaged<MTAudioProcessingTap>?>) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func MTAudioProcessingTapGetStorage(_ tap: MTAudioProcessingTap) -> UnsafeMutablePointer<Void>
@available(iOS 6.0, *)
@discardableResult
func MTAudioProcessingTapGetSourceAudio(_ tap: MTAudioProcessingTap, _ numberFrames: CMItemCount, _ bufferListInOut: UnsafeMutablePointer<AudioBufferList>, _ flagsOut: UnsafeMutablePointer<MTAudioProcessingTapFlags>?, _ timeRangeOut: UnsafeMutablePointer<CMTimeRange>?, _ numberFramesOut: UnsafeMutablePointer<CMItemCount>?) -> OSStatus
