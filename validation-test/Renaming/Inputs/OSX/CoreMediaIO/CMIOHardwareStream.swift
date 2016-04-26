
typealias CMIOStreamID = CMIOObjectID
typealias CMIODeviceStreamQueueAlteredProc = @convention(c) (CMIOStreamID, UnsafeMutablePointer<Void>!, UnsafeMutablePointer<Void>!) -> Void
struct CMIOStreamDeck {
  var mStatus: UInt32
  var mState: UInt32
  var mState2: UInt32
  init()
  init(mStatus mStatus: UInt32, mState mState: UInt32, mState2 mState2: UInt32)
}
var kCMIOStreamClassID: Int { get }
var kCMIOStreamUnknown: Int { get }
var kCMIODeckStatusBusy: Int { get }
var kCMIODeckStatusLocal: Int { get }
var kCMIODeckStatusNotThreaded: Int { get }
var kCMIODeckStatusTapeInserted: Int { get }
var kCMIODeckStatusOpcode: Int { get }
var kCMIODeckStatusSearchingForDevice: Int { get }
var kCMIODeckStatusNoDevice: Int { get }
var kCMIODeckStateStop: Int { get }
var kCMIODeckStatePlay: Int { get }
var kCMIODeckStatePause: Int { get }
var kCMIODeckStatePlaySlow: Int { get }
var kCMIODeckStateReverseSlow: Int { get }
var kCMIODeckStatePlayReverse: Int { get }
var kCMIODeckStateFastForward: Int { get }
var kCMIODeckStateFastRewind: Int { get }
var kCMIODeckShuttleReverseHighSpeed: Int { get }
var kCMIODeckShuttleReverseFastest: Int { get }
var kCMIODeckShuttleReverseFaster: Int { get }
var kCMIODeckShuttleReverseFast: Int { get }
var kCMIODeckShuttleReverse1x: Int { get }
var kCMIODeckShuttleReverseSlow3: Int { get }
var kCMIODeckShuttleReverseSlow2: Int { get }
var kCMIODeckShuttleReverseSlow1: Int { get }
var kCMIODeckShuttleReverseSlowest: Int { get }
var kCMIODeckShuttlePlayPreviousFrame: Int { get }
var kCMIODeckShuttlePause: Int { get }
var kCMIODeckShuttlePlayNextFrame: Int { get }
var kCMIODeckShuttlePlaySlowest: Int { get }
var kCMIODeckShuttlePlaySlow1: Int { get }
var kCMIODeckShuttlePlaySlow2: Int { get }
var kCMIODeckShuttlePlaySlow3: Int { get }
var kCMIODeckShuttlePlay1x: Int { get }
var kCMIODeckShuttlePlayFast: Int { get }
var kCMIODeckShuttlePlayFaster: Int { get }
var kCMIODeckShuttlePlayFastest: Int { get }
var kCMIODeckShuttlePlayHighSpeed: Int { get }
var kCMIOStreamPropertyDirection: Int { get }
var kCMIOStreamPropertyTerminalType: Int { get }
var kCMIOStreamPropertyStartingChannel: Int { get }
var kCMIOStreamPropertyLatency: Int { get }
var kCMIOStreamPropertyFormatDescription: Int { get }
var kCMIOStreamPropertyFormatDescriptions: Int { get }
var kCMIOStreamPropertyStillImage: Int { get }
var kCMIOStreamPropertyStillImageFormatDescriptions: Int { get }
var kCMIOStreamPropertyFrameRate: Int { get }
var kCMIOStreamPropertyMinimumFrameRate: Int { get }
var kCMIOStreamPropertyFrameRates: Int { get }
var kCMIOStreamPropertyFrameRateRanges: Int { get }
var kCMIOStreamPropertyNoDataTimeoutInMSec: Int { get }
var kCMIOStreamPropertyDeviceSyncTimeoutInMSec: Int { get }
var kCMIOStreamPropertyNoDataEventCount: Int { get }
var kCMIOStreamPropertyOutputBufferUnderrunCount: Int { get }
var kCMIOStreamPropertyOutputBufferRepeatCount: Int { get }
var kCMIOStreamPropertyOutputBufferQueueSize: Int { get }
var kCMIOStreamPropertyOutputBuffersRequiredForStartup: Int { get }
var kCMIOStreamPropertyOutputBuffersNeededForThrottledPlayback: Int { get }
var kCMIOStreamPropertyFirstOutputPresentationTimeStamp: Int { get }
var kCMIOStreamPropertyEndOfData: Int { get }
var kCMIOStreamPropertyClock: Int { get }
var kCMIOStreamPropertyCanProcessDeckCommand: Int { get }
var kCMIOStreamPropertyDeck: Int { get }
var kCMIOStreamPropertyDeckFrameNumber: Int { get }
var kCMIOStreamPropertyDeckDropness: Int { get }
var kCMIOStreamPropertyDeckThreaded: Int { get }
var kCMIOStreamPropertyDeckLocal: Int { get }
var kCMIOStreamPropertyDeckCueing: Int { get }
var kCMIOStreamPropertyInitialPresentationTimeStampForLinkedAndSyncedAudio: Int { get }
var kCMIOStreamPropertyScheduledOutputNotificationProc: Int { get }
var kCMIOStreamPropertyPreferredFormatDescription: Int { get }
var kCMIOStreamPropertyPreferredFrameRate: Int { get }
@available(OSX 10.7, *)
@discardableResult
func CMIOStreamCopyBufferQueue(_ streamID: CMIOStreamID, _ queueAlteredProc: CMIODeviceStreamQueueAlteredProc!, _ queueAlteredRefCon: UnsafeMutablePointer<Void>!, _ queue: UnsafeMutablePointer<Unmanaged<CMSimpleQueue>?>!) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMIOStreamDeckPlay(_ streamID: CMIOStreamID) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMIOStreamDeckStop(_ streamID: CMIOStreamID) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMIOStreamDeckJog(_ streamID: CMIOStreamID, _ speed: Int32) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMIOStreamDeckCueTo(_ streamID: CMIOStreamID, _ frameNumber: UInt64, _ playOnCue: Bool) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMIOStreamClockCreate(_ allocator: CFAllocator!, _ clockName: CFString!, _ sourceIdentifier: UnsafePointer<Void>!, _ getTimeCallMinimumInterval: CMTime, _ numberOfEventsForRateSmoothing: UInt32, _ numberOfAveragesForRateSmoothing: UInt32, _ clock: UnsafeMutablePointer<Unmanaged<CFTypeRef>?>!) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMIOStreamClockPostTimingEvent(_ eventTime: CMTime, _ hostTime: UInt64, _ resynchronize: Bool, _ clock: CFTypeRef!) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMIOStreamClockInvalidate(_ clock: CFTypeRef!) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMIOStreamClockConvertHostTimeToDeviceTime(_ hostTime: UInt64, _ clock: CFTypeRef!) -> CMTime
typealias CMIOStreamScheduledOutputNotificationProc = @convention(c) (UInt64, UInt64, UnsafeMutablePointer<Void>!) -> Void
struct CMIOStreamScheduledOutputNotificationProcAndRefCon {
  var scheduledOutputNotificationProc: CMIOStreamScheduledOutputNotificationProc!
  var scheduledOutputNotificationRefCon: UnsafeMutablePointer<Void>!
  init()
  init(scheduledOutputNotificationProc scheduledOutputNotificationProc: CMIOStreamScheduledOutputNotificationProc!, scheduledOutputNotificationRefCon scheduledOutputNotificationRefCon: UnsafeMutablePointer<Void>!)
}
