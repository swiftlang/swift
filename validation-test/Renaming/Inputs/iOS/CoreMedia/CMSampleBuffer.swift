
var kCMSampleBufferError_AllocationFailed: OSStatus { get }
var kCMSampleBufferError_RequiredParameterMissing: OSStatus { get }
var kCMSampleBufferError_AlreadyHasDataBuffer: OSStatus { get }
var kCMSampleBufferError_BufferNotReady: OSStatus { get }
var kCMSampleBufferError_SampleIndexOutOfRange: OSStatus { get }
var kCMSampleBufferError_BufferHasNoSampleSizes: OSStatus { get }
var kCMSampleBufferError_BufferHasNoSampleTimingInfo: OSStatus { get }
var kCMSampleBufferError_ArrayTooSmall: OSStatus { get }
var kCMSampleBufferError_InvalidEntryCount: OSStatus { get }
var kCMSampleBufferError_CannotSubdivide: OSStatus { get }
var kCMSampleBufferError_SampleTimingInfoInvalid: OSStatus { get }
var kCMSampleBufferError_InvalidMediaTypeForOperation: OSStatus { get }
var kCMSampleBufferError_InvalidSampleData: OSStatus { get }
var kCMSampleBufferError_InvalidMediaFormat: OSStatus { get }
var kCMSampleBufferError_Invalidated: OSStatus { get }
var kCMSampleBufferError_DataFailed: OSStatus { get }
var kCMSampleBufferError_DataCanceled: OSStatus { get }
var kCMSampleBufferFlag_AudioBufferList_Assure16ByteAlignment: UInt32 { get }
class CMSampleBuffer {
}
struct CMSampleTimingInfo {
  var duration: CMTime
  var presentationTimeStamp: CMTime
  var decodeTimeStamp: CMTime
  init()
  init(duration duration: CMTime, presentationTimeStamp presentationTimeStamp: CMTime, decodeTimeStamp decodeTimeStamp: CMTime)
}
@available(iOS 4.0, *)
let kCMTimingInfoInvalid: CMSampleTimingInfo
typealias CMSampleBufferMakeDataReadyCallback = @convention(c) (CMSampleBuffer, UnsafeMutablePointer<Void>?) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferCreate(_ allocator: CFAllocator?, _ dataBuffer: CMBlockBuffer?, _ dataReady: Bool, _ makeDataReadyCallback: CMSampleBufferMakeDataReadyCallback?, _ makeDataReadyRefcon: UnsafeMutablePointer<Void>?, _ formatDescription: CMFormatDescription?, _ numSamples: CMItemCount, _ numSampleTimingEntries: CMItemCount, _ sampleTimingArray: UnsafePointer<CMSampleTimingInfo>?, _ numSampleSizeEntries: CMItemCount, _ sampleSizeArray: UnsafePointer<Int>?, _ sBufOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(iOS 8.0, *)
@discardableResult
func CMSampleBufferCreateReady(_ allocator: CFAllocator?, _ dataBuffer: CMBlockBuffer?, _ formatDescription: CMFormatDescription?, _ numSamples: CMItemCount, _ numSampleTimingEntries: CMItemCount, _ sampleTimingArray: UnsafePointer<CMSampleTimingInfo>?, _ numSampleSizeEntries: CMItemCount, _ sampleSizeArray: UnsafePointer<Int>?, _ sBufOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMAudioSampleBufferCreateWithPacketDescriptions(_ allocator: CFAllocator?, _ dataBuffer: CMBlockBuffer?, _ dataReady: Bool, _ makeDataReadyCallback: CMSampleBufferMakeDataReadyCallback?, _ makeDataReadyRefcon: UnsafeMutablePointer<Void>?, _ formatDescription: CMFormatDescription, _ numSamples: CMItemCount, _ sbufPTS: CMTime, _ packetDescriptions: UnsafePointer<AudioStreamPacketDescription>?, _ sBufOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(iOS 8.0, *)
@discardableResult
func CMAudioSampleBufferCreateReadyWithPacketDescriptions(_ allocator: CFAllocator?, _ dataBuffer: CMBlockBuffer?, _ formatDescription: CMFormatDescription, _ numSamples: CMItemCount, _ sbufPTS: CMTime, _ packetDescriptions: UnsafePointer<AudioStreamPacketDescription>?, _ sBufOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferCreateForImageBuffer(_ allocator: CFAllocator?, _ imageBuffer: CVImageBuffer, _ dataReady: Bool, _ makeDataReadyCallback: CMSampleBufferMakeDataReadyCallback?, _ makeDataReadyRefcon: UnsafeMutablePointer<Void>?, _ formatDescription: CMVideoFormatDescription, _ sampleTiming: UnsafePointer<CMSampleTimingInfo>, _ sBufOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(iOS 8.0, *)
@discardableResult
func CMSampleBufferCreateReadyWithImageBuffer(_ allocator: CFAllocator?, _ imageBuffer: CVImageBuffer, _ formatDescription: CMVideoFormatDescription, _ sampleTiming: UnsafePointer<CMSampleTimingInfo>, _ sBufOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferCreateCopy(_ allocator: CFAllocator?, _ sbuf: CMSampleBuffer, _ sbufCopyOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferCreateCopyWithNewTiming(_ allocator: CFAllocator?, _ originalSBuf: CMSampleBuffer, _ numSampleTimingEntries: CMItemCount, _ sampleTimingArray: UnsafePointer<CMSampleTimingInfo>?, _ sBufCopyOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferCopySampleBufferForRange(_ allocator: CFAllocator?, _ sbuf: CMSampleBuffer, _ sampleRange: CFRange, _ sBufOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetTypeID() -> CFTypeID
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferSetDataBuffer(_ sbuf: CMSampleBuffer, _ dataBuffer: CMBlockBuffer) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetDataBuffer(_ sbuf: CMSampleBuffer) -> CMBlockBuffer?
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetImageBuffer(_ sbuf: CMSampleBuffer) -> CVImageBuffer?
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferSetDataBufferFromAudioBufferList(_ sbuf: CMSampleBuffer, _ bbufStructAllocator: CFAllocator?, _ bbufMemoryAllocator: CFAllocator?, _ flags: UInt32, _ bufferList: UnsafePointer<AudioBufferList>) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetAudioBufferListWithRetainedBlockBuffer(_ sbuf: CMSampleBuffer, _ bufferListSizeNeededOut: UnsafeMutablePointer<Int>?, _ bufferListOut: UnsafeMutablePointer<AudioBufferList>?, _ bufferListSize: Int, _ bbufStructAllocator: CFAllocator?, _ bbufMemoryAllocator: CFAllocator?, _ flags: UInt32, _ blockBufferOut: UnsafeMutablePointer<CMBlockBuffer?>?) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetAudioStreamPacketDescriptions(_ sbuf: CMSampleBuffer, _ packetDescriptionsSize: Int, _ packetDescriptionsOut: UnsafeMutablePointer<AudioStreamPacketDescription>?, _ packetDescriptionsSizeNeededOut: UnsafeMutablePointer<Int>?) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetAudioStreamPacketDescriptionsPtr(_ sbuf: CMSampleBuffer, _ packetDescriptionsPtrOut: UnsafeMutablePointer<UnsafePointer<AudioStreamPacketDescription>?>?, _ packetDescriptionsSizeOut: UnsafeMutablePointer<Int>?) -> OSStatus
@available(iOS 7.0, *)
@discardableResult
func CMSampleBufferCopyPCMDataIntoAudioBufferList(_ sbuf: CMSampleBuffer, _ frameOffset: Int32, _ numFrames: Int32, _ bufferList: UnsafeMutablePointer<AudioBufferList>) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferSetDataReady(_ sbuf: CMSampleBuffer) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferDataIsReady(_ sbuf: CMSampleBuffer) -> Bool
@available(iOS 8.0, *)
@discardableResult
func CMSampleBufferSetDataFailed(_ sbuf: CMSampleBuffer, _ status: OSStatus) -> OSStatus
@available(iOS 8.0, *)
@discardableResult
func CMSampleBufferHasDataFailed(_ sbuf: CMSampleBuffer, _ statusOut: UnsafeMutablePointer<OSStatus>) -> Bool
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferMakeDataReady(_ sbuf: CMSampleBuffer) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferTrackDataReadiness(_ sbuf: CMSampleBuffer, _ sbufToTrack: CMSampleBuffer) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferInvalidate(_ sbuf: CMSampleBuffer) -> OSStatus
typealias CMSampleBufferInvalidateCallback = @convention(c) (CMSampleBuffer, UInt64) -> Void
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferSetInvalidateCallback(_ sbuf: CMSampleBuffer, _ invalidateCallback: CMSampleBufferInvalidateCallback, _ invalidateRefCon: UInt64) -> OSStatus
typealias CMSampleBufferInvalidateHandler = (CMSampleBuffer) -> Void
@available(iOS 8.0, *)
@discardableResult
func CMSampleBufferSetInvalidateHandler(_ sbuf: CMSampleBuffer, _ invalidateHandler: CMSampleBufferInvalidateHandler) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferIsValid(_ sbuf: CMSampleBuffer) -> Bool
@available(iOS 4.0, *)
let kCMSampleBufferNotification_DataBecameReady: CFString
@available(iOS 8.0, *)
let kCMSampleBufferNotification_DataFailed: CFString
@available(iOS 8.0, *)
let kCMSampleBufferNotificationParameter_OSStatus: CFString
@available(iOS 4.0, *)
let kCMSampleBufferConduitNotification_InhibitOutputUntil: CFString
@available(iOS 4.0, *)
let kCMSampleBufferConduitNotificationParameter_ResumeTag: CFString
@available(iOS 4.0, *)
let kCMSampleBufferConduitNotification_ResetOutput: CFString
@available(iOS 4.3, *)
let kCMSampleBufferConduitNotification_UpcomingOutputPTSRangeChanged: CFString
@available(iOS 4.3, *)
let kCMSampleBufferConduitNotificationParameter_UpcomingOutputPTSRangeMayOverlapQueuedOutputPTSRange: CFString
@available(iOS 4.3, *)
let kCMSampleBufferConduitNotificationParameter_MinUpcomingOutputPTS: CFString
@available(iOS 5.0, *)
let kCMSampleBufferConduitNotificationParameter_MaxUpcomingOutputPTS: CFString
@available(iOS 4.0, *)
let kCMSampleBufferConsumerNotification_BufferConsumed: CFString
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetNumSamples(_ sbuf: CMSampleBuffer) -> CMItemCount
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetDuration(_ sbuf: CMSampleBuffer) -> CMTime
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetPresentationTimeStamp(_ sbuf: CMSampleBuffer) -> CMTime
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetDecodeTimeStamp(_ sbuf: CMSampleBuffer) -> CMTime
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetOutputDuration(_ sbuf: CMSampleBuffer) -> CMTime
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetOutputPresentationTimeStamp(_ sbuf: CMSampleBuffer) -> CMTime
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferSetOutputPresentationTimeStamp(_ sbuf: CMSampleBuffer, _ outputPresentationTimeStamp: CMTime) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetOutputDecodeTimeStamp(_ sbuf: CMSampleBuffer) -> CMTime
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetSampleTimingInfoArray(_ sbuf: CMSampleBuffer, _ timingArrayEntries: CMItemCount, _ timingArrayOut: UnsafeMutablePointer<CMSampleTimingInfo>?, _ timingArrayEntriesNeededOut: UnsafeMutablePointer<CMItemCount>?) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetOutputSampleTimingInfoArray(_ sbuf: CMSampleBuffer, _ timingArrayEntries: CMItemCount, _ timingArrayOut: UnsafeMutablePointer<CMSampleTimingInfo>?, _ timingArrayEntriesNeededOut: UnsafeMutablePointer<CMItemCount>?) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetSampleTimingInfo(_ sbuf: CMSampleBuffer, _ sampleIndex: CMItemIndex, _ timingInfoOut: UnsafeMutablePointer<CMSampleTimingInfo>) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetSampleSizeArray(_ sbuf: CMSampleBuffer, _ sizeArrayEntries: CMItemCount, _ sizeArrayOut: UnsafeMutablePointer<Int>?, _ sizeArrayEntriesNeededOut: UnsafeMutablePointer<CMItemCount>?) -> OSStatus
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetSampleSize(_ sbuf: CMSampleBuffer, _ sampleIndex: CMItemIndex) -> Int
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetTotalSampleSize(_ sbuf: CMSampleBuffer) -> Int
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetFormatDescription(_ sbuf: CMSampleBuffer) -> CMFormatDescription?
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferGetSampleAttachmentsArray(_ sbuf: CMSampleBuffer, _ createIfNecessary: Bool) -> CFArray?
@available(iOS 4.0, *)
let kCMSampleAttachmentKey_NotSync: CFString
@available(iOS 4.0, *)
let kCMSampleAttachmentKey_PartialSync: CFString
@available(iOS 4.0, *)
let kCMSampleAttachmentKey_HasRedundantCoding: CFString
@available(iOS 4.0, *)
let kCMSampleAttachmentKey_IsDependedOnByOthers: CFString
@available(iOS 4.0, *)
let kCMSampleAttachmentKey_DependsOnOthers: CFString
@available(iOS 4.0, *)
let kCMSampleAttachmentKey_EarlierDisplayTimesAllowed: CFString
@available(iOS 4.0, *)
let kCMSampleAttachmentKey_DisplayImmediately: CFString
@available(iOS 4.0, *)
let kCMSampleAttachmentKey_DoNotDisplay: CFString
@available(iOS 4.0, *)
let kCMSampleBufferAttachmentKey_ResetDecoderBeforeDecoding: CFString
@available(iOS 4.0, *)
let kCMSampleBufferAttachmentKey_DrainAfterDecoding: CFString
@available(iOS 4.0, *)
let kCMSampleBufferAttachmentKey_PostNotificationWhenConsumed: CFString
@available(iOS 4.0, *)
let kCMSampleBufferAttachmentKey_ResumeOutput: CFString
@available(iOS 4.0, *)
let kCMSampleBufferAttachmentKey_TransitionID: CFString
@available(iOS 4.0, *)
let kCMSampleBufferAttachmentKey_TrimDurationAtStart: CFString
@available(iOS 4.0, *)
let kCMSampleBufferAttachmentKey_TrimDurationAtEnd: CFString
@available(iOS 4.0, *)
let kCMSampleBufferAttachmentKey_SpeedMultiplier: CFString
@available(iOS 4.0, *)
let kCMSampleBufferAttachmentKey_Reverse: CFString
@available(iOS 4.0, *)
let kCMSampleBufferAttachmentKey_FillDiscontinuitiesWithSilence: CFString
@available(iOS 4.0, *)
let kCMSampleBufferAttachmentKey_EmptyMedia: CFString
@available(iOS 4.0, *)
let kCMSampleBufferAttachmentKey_PermanentEmptyMedia: CFString
@available(iOS 4.0, *)
let kCMSampleBufferAttachmentKey_DisplayEmptyMediaImmediately: CFString
@available(iOS 4.0, *)
let kCMSampleBufferAttachmentKey_EndsPreviousSampleDuration: CFString
@available(iOS 4.0, *)
let kCMSampleBufferAttachmentKey_SampleReferenceURL: CFString
@available(iOS 4.0, *)
let kCMSampleBufferAttachmentKey_SampleReferenceByteOffset: CFString
@available(iOS 4.3, *)
let kCMSampleBufferAttachmentKey_GradualDecoderRefresh: CFString
@available(iOS 6.0, *)
let kCMSampleBufferAttachmentKey_DroppedFrameReason: CFString
@available(iOS 6.0, *)
let kCMSampleBufferDroppedFrameReason_FrameWasLate: CFString
@available(iOS 6.0, *)
let kCMSampleBufferDroppedFrameReason_OutOfBuffers: CFString
@available(iOS 6.0, *)
let kCMSampleBufferDroppedFrameReason_Discontinuity: CFString
@available(iOS 7.0, *)
let kCMSampleBufferAttachmentKey_DroppedFrameReasonInfo: CFString
@available(iOS 7.0, *)
let kCMSampleBufferDroppedFrameReasonInfo_CameraModeSwitch: CFString
@available(iOS 9.0, *)
let kCMSampleBufferAttachmentKey_StillImageLensStabilizationInfo: CFString
@available(iOS 9.0, *)
let kCMSampleBufferLensStabilizationInfo_Active: CFString
@available(iOS 9.0, *)
let kCMSampleBufferLensStabilizationInfo_OutOfRange: CFString
@available(iOS 9.0, *)
let kCMSampleBufferLensStabilizationInfo_Unavailable: CFString
@available(iOS 9.0, *)
let kCMSampleBufferLensStabilizationInfo_Off: CFString
@available(iOS 8.0, *)
let kCMSampleBufferAttachmentKey_ForceKeyFrame: CFString
@available(iOS 4.0, *)
@discardableResult
func CMSampleBufferCallForEachSample(_ sbuf: CMSampleBuffer, _ callback: @convention(c) (CMSampleBuffer, CMItemCount, UnsafeMutablePointer<Void>?) -> OSStatus, _ refcon: UnsafeMutablePointer<Void>?) -> OSStatus
@available(iOS 8.0, *)
@discardableResult
func CMSampleBufferCallBlockForEachSample(_ sbuf: CMSampleBuffer, _ handler: (CMSampleBuffer, CMItemCount) -> OSStatus) -> OSStatus
