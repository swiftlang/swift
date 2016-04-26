
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
@available(OSX 10.7, *)
let kCMTimingInfoInvalid: CMSampleTimingInfo
typealias CMSampleBufferMakeDataReadyCallback = @convention(c) (CMSampleBuffer, UnsafeMutablePointer<Void>?) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferCreate(_ allocator: CFAllocator?, _ dataBuffer: CMBlockBuffer?, _ dataReady: Bool, _ makeDataReadyCallback: CMSampleBufferMakeDataReadyCallback?, _ makeDataReadyRefcon: UnsafeMutablePointer<Void>?, _ formatDescription: CMFormatDescription?, _ numSamples: CMItemCount, _ numSampleTimingEntries: CMItemCount, _ sampleTimingArray: UnsafePointer<CMSampleTimingInfo>?, _ numSampleSizeEntries: CMItemCount, _ sampleSizeArray: UnsafePointer<Int>?, _ sBufOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(OSX 10.10, *)
@discardableResult
func CMSampleBufferCreateReady(_ allocator: CFAllocator?, _ dataBuffer: CMBlockBuffer?, _ formatDescription: CMFormatDescription?, _ numSamples: CMItemCount, _ numSampleTimingEntries: CMItemCount, _ sampleTimingArray: UnsafePointer<CMSampleTimingInfo>?, _ numSampleSizeEntries: CMItemCount, _ sampleSizeArray: UnsafePointer<Int>?, _ sBufOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMAudioSampleBufferCreateWithPacketDescriptions(_ allocator: CFAllocator?, _ dataBuffer: CMBlockBuffer?, _ dataReady: Bool, _ makeDataReadyCallback: CMSampleBufferMakeDataReadyCallback?, _ makeDataReadyRefcon: UnsafeMutablePointer<Void>?, _ formatDescription: CMFormatDescription, _ numSamples: CMItemCount, _ sbufPTS: CMTime, _ packetDescriptions: UnsafePointer<AudioStreamPacketDescription>?, _ sBufOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(OSX 10.10, *)
@discardableResult
func CMAudioSampleBufferCreateReadyWithPacketDescriptions(_ allocator: CFAllocator?, _ dataBuffer: CMBlockBuffer?, _ formatDescription: CMFormatDescription, _ numSamples: CMItemCount, _ sbufPTS: CMTime, _ packetDescriptions: UnsafePointer<AudioStreamPacketDescription>?, _ sBufOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferCreateForImageBuffer(_ allocator: CFAllocator?, _ imageBuffer: CVImageBuffer, _ dataReady: Bool, _ makeDataReadyCallback: CMSampleBufferMakeDataReadyCallback?, _ makeDataReadyRefcon: UnsafeMutablePointer<Void>?, _ formatDescription: CMVideoFormatDescription, _ sampleTiming: UnsafePointer<CMSampleTimingInfo>, _ sBufOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(OSX 10.10, *)
@discardableResult
func CMSampleBufferCreateReadyWithImageBuffer(_ allocator: CFAllocator?, _ imageBuffer: CVImageBuffer, _ formatDescription: CMVideoFormatDescription, _ sampleTiming: UnsafePointer<CMSampleTimingInfo>, _ sBufOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferCreateCopy(_ allocator: CFAllocator?, _ sbuf: CMSampleBuffer, _ sbufCopyOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferCreateCopyWithNewTiming(_ allocator: CFAllocator?, _ originalSBuf: CMSampleBuffer, _ numSampleTimingEntries: CMItemCount, _ sampleTimingArray: UnsafePointer<CMSampleTimingInfo>?, _ sBufCopyOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferCopySampleBufferForRange(_ allocator: CFAllocator?, _ sbuf: CMSampleBuffer, _ sampleRange: CFRange, _ sBufOut: UnsafeMutablePointer<CMSampleBuffer?>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetTypeID() -> CFTypeID
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferSetDataBuffer(_ sbuf: CMSampleBuffer, _ dataBuffer: CMBlockBuffer) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetDataBuffer(_ sbuf: CMSampleBuffer) -> CMBlockBuffer?
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetImageBuffer(_ sbuf: CMSampleBuffer) -> CVImageBuffer?
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferSetDataBufferFromAudioBufferList(_ sbuf: CMSampleBuffer, _ bbufStructAllocator: CFAllocator?, _ bbufMemoryAllocator: CFAllocator?, _ flags: UInt32, _ bufferList: UnsafePointer<AudioBufferList>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetAudioBufferListWithRetainedBlockBuffer(_ sbuf: CMSampleBuffer, _ bufferListSizeNeededOut: UnsafeMutablePointer<Int>?, _ bufferListOut: UnsafeMutablePointer<AudioBufferList>?, _ bufferListSize: Int, _ bbufStructAllocator: CFAllocator?, _ bbufMemoryAllocator: CFAllocator?, _ flags: UInt32, _ blockBufferOut: UnsafeMutablePointer<CMBlockBuffer?>?) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetAudioStreamPacketDescriptions(_ sbuf: CMSampleBuffer, _ packetDescriptionsSize: Int, _ packetDescriptionsOut: UnsafeMutablePointer<AudioStreamPacketDescription>?, _ packetDescriptionsSizeNeededOut: UnsafeMutablePointer<Int>?) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetAudioStreamPacketDescriptionsPtr(_ sbuf: CMSampleBuffer, _ packetDescriptionsPtrOut: UnsafeMutablePointer<UnsafePointer<AudioStreamPacketDescription>?>?, _ packetDescriptionsSizeOut: UnsafeMutablePointer<Int>?) -> OSStatus
@available(OSX 10.9, *)
@discardableResult
func CMSampleBufferCopyPCMDataIntoAudioBufferList(_ sbuf: CMSampleBuffer, _ frameOffset: Int32, _ numFrames: Int32, _ bufferList: UnsafeMutablePointer<AudioBufferList>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferSetDataReady(_ sbuf: CMSampleBuffer) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferDataIsReady(_ sbuf: CMSampleBuffer) -> Bool
@available(OSX 10.10, *)
@discardableResult
func CMSampleBufferSetDataFailed(_ sbuf: CMSampleBuffer, _ status: OSStatus) -> OSStatus
@available(OSX 10.10, *)
@discardableResult
func CMSampleBufferHasDataFailed(_ sbuf: CMSampleBuffer, _ statusOut: UnsafeMutablePointer<OSStatus>) -> Bool
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferMakeDataReady(_ sbuf: CMSampleBuffer) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferTrackDataReadiness(_ sbuf: CMSampleBuffer, _ sbufToTrack: CMSampleBuffer) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferInvalidate(_ sbuf: CMSampleBuffer) -> OSStatus
typealias CMSampleBufferInvalidateCallback = @convention(c) (CMSampleBuffer, UInt64) -> Void
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferSetInvalidateCallback(_ sbuf: CMSampleBuffer, _ invalidateCallback: CMSampleBufferInvalidateCallback, _ invalidateRefCon: UInt64) -> OSStatus
typealias CMSampleBufferInvalidateHandler = (CMSampleBuffer) -> Void
@available(OSX 10.10, *)
@discardableResult
func CMSampleBufferSetInvalidateHandler(_ sbuf: CMSampleBuffer, _ invalidateHandler: CMSampleBufferInvalidateHandler) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferIsValid(_ sbuf: CMSampleBuffer) -> Bool
@available(OSX 10.7, *)
let kCMSampleBufferNotification_DataBecameReady: CFString
@available(OSX 10.10, *)
let kCMSampleBufferNotification_DataFailed: CFString
@available(OSX 10.10, *)
let kCMSampleBufferNotificationParameter_OSStatus: CFString
@available(OSX 10.7, *)
let kCMSampleBufferConduitNotification_InhibitOutputUntil: CFString
@available(OSX 10.7, *)
let kCMSampleBufferConduitNotificationParameter_ResumeTag: CFString
@available(OSX 10.7, *)
let kCMSampleBufferConduitNotification_ResetOutput: CFString
@available(OSX 10.7, *)
let kCMSampleBufferConduitNotification_UpcomingOutputPTSRangeChanged: CFString
@available(OSX 10.7, *)
let kCMSampleBufferConduitNotificationParameter_UpcomingOutputPTSRangeMayOverlapQueuedOutputPTSRange: CFString
@available(OSX 10.7, *)
let kCMSampleBufferConduitNotificationParameter_MinUpcomingOutputPTS: CFString
@available(OSX 10.8, *)
let kCMSampleBufferConduitNotificationParameter_MaxUpcomingOutputPTS: CFString
@available(OSX 10.7, *)
let kCMSampleBufferConsumerNotification_BufferConsumed: CFString
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetNumSamples(_ sbuf: CMSampleBuffer) -> CMItemCount
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetDuration(_ sbuf: CMSampleBuffer) -> CMTime
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetPresentationTimeStamp(_ sbuf: CMSampleBuffer) -> CMTime
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetDecodeTimeStamp(_ sbuf: CMSampleBuffer) -> CMTime
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetOutputDuration(_ sbuf: CMSampleBuffer) -> CMTime
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetOutputPresentationTimeStamp(_ sbuf: CMSampleBuffer) -> CMTime
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferSetOutputPresentationTimeStamp(_ sbuf: CMSampleBuffer, _ outputPresentationTimeStamp: CMTime) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetOutputDecodeTimeStamp(_ sbuf: CMSampleBuffer) -> CMTime
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetSampleTimingInfoArray(_ sbuf: CMSampleBuffer, _ timingArrayEntries: CMItemCount, _ timingArrayOut: UnsafeMutablePointer<CMSampleTimingInfo>?, _ timingArrayEntriesNeededOut: UnsafeMutablePointer<CMItemCount>?) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetOutputSampleTimingInfoArray(_ sbuf: CMSampleBuffer, _ timingArrayEntries: CMItemCount, _ timingArrayOut: UnsafeMutablePointer<CMSampleTimingInfo>?, _ timingArrayEntriesNeededOut: UnsafeMutablePointer<CMItemCount>?) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetSampleTimingInfo(_ sbuf: CMSampleBuffer, _ sampleIndex: CMItemIndex, _ timingInfoOut: UnsafeMutablePointer<CMSampleTimingInfo>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetSampleSizeArray(_ sbuf: CMSampleBuffer, _ sizeArrayEntries: CMItemCount, _ sizeArrayOut: UnsafeMutablePointer<Int>?, _ sizeArrayEntriesNeededOut: UnsafeMutablePointer<CMItemCount>?) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetSampleSize(_ sbuf: CMSampleBuffer, _ sampleIndex: CMItemIndex) -> Int
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetTotalSampleSize(_ sbuf: CMSampleBuffer) -> Int
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetFormatDescription(_ sbuf: CMSampleBuffer) -> CMFormatDescription?
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferGetSampleAttachmentsArray(_ sbuf: CMSampleBuffer, _ createIfNecessary: Bool) -> CFArray?
@available(OSX 10.7, *)
let kCMSampleAttachmentKey_NotSync: CFString
@available(OSX 10.7, *)
let kCMSampleAttachmentKey_PartialSync: CFString
@available(OSX 10.7, *)
let kCMSampleAttachmentKey_HasRedundantCoding: CFString
@available(OSX 10.7, *)
let kCMSampleAttachmentKey_IsDependedOnByOthers: CFString
@available(OSX 10.7, *)
let kCMSampleAttachmentKey_DependsOnOthers: CFString
@available(OSX 10.7, *)
let kCMSampleAttachmentKey_EarlierDisplayTimesAllowed: CFString
@available(OSX 10.7, *)
let kCMSampleAttachmentKey_DisplayImmediately: CFString
@available(OSX 10.7, *)
let kCMSampleAttachmentKey_DoNotDisplay: CFString
@available(OSX 10.7, *)
let kCMSampleBufferAttachmentKey_ResetDecoderBeforeDecoding: CFString
@available(OSX 10.7, *)
let kCMSampleBufferAttachmentKey_DrainAfterDecoding: CFString
@available(OSX 10.7, *)
let kCMSampleBufferAttachmentKey_PostNotificationWhenConsumed: CFString
@available(OSX 10.7, *)
let kCMSampleBufferAttachmentKey_ResumeOutput: CFString
@available(OSX 10.7, *)
let kCMSampleBufferAttachmentKey_TransitionID: CFString
@available(OSX 10.7, *)
let kCMSampleBufferAttachmentKey_TrimDurationAtStart: CFString
@available(OSX 10.7, *)
let kCMSampleBufferAttachmentKey_TrimDurationAtEnd: CFString
@available(OSX 10.7, *)
let kCMSampleBufferAttachmentKey_SpeedMultiplier: CFString
@available(OSX 10.7, *)
let kCMSampleBufferAttachmentKey_Reverse: CFString
@available(OSX 10.7, *)
let kCMSampleBufferAttachmentKey_FillDiscontinuitiesWithSilence: CFString
@available(OSX 10.7, *)
let kCMSampleBufferAttachmentKey_EmptyMedia: CFString
@available(OSX 10.7, *)
let kCMSampleBufferAttachmentKey_PermanentEmptyMedia: CFString
@available(OSX 10.7, *)
let kCMSampleBufferAttachmentKey_DisplayEmptyMediaImmediately: CFString
@available(OSX 10.7, *)
let kCMSampleBufferAttachmentKey_EndsPreviousSampleDuration: CFString
@available(OSX 10.7, *)
let kCMSampleBufferAttachmentKey_SampleReferenceURL: CFString
@available(OSX 10.7, *)
let kCMSampleBufferAttachmentKey_SampleReferenceByteOffset: CFString
@available(OSX 10.7, *)
let kCMSampleBufferAttachmentKey_GradualDecoderRefresh: CFString
@available(OSX 10.10, *)
let kCMSampleBufferAttachmentKey_ForceKeyFrame: CFString
@available(OSX 10.7, *)
@discardableResult
func CMSampleBufferCallForEachSample(_ sbuf: CMSampleBuffer, _ callback: @convention(c) (CMSampleBuffer, CMItemCount, UnsafeMutablePointer<Void>?) -> OSStatus, _ refcon: UnsafeMutablePointer<Void>?) -> OSStatus
@available(OSX 10.10, *)
@discardableResult
func CMSampleBufferCallBlockForEachSample(_ sbuf: CMSampleBuffer, _ handler: (CMSampleBuffer, CMItemCount) -> OSStatus) -> OSStatus
