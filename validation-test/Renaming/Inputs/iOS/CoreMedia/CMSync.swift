
class CMClock {
}
class CMTimebase {
}
typealias CMClockOrTimebase = CFTypeRef
var kCMClockError_MissingRequiredParameter: OSStatus { get }
var kCMClockError_InvalidParameter: OSStatus { get }
var kCMClockError_AllocationFailed: OSStatus { get }
var kCMClockError_UnsupportedOperation: OSStatus { get }
var kCMTimebaseError_MissingRequiredParameter: OSStatus { get }
var kCMTimebaseError_InvalidParameter: OSStatus { get }
var kCMTimebaseError_AllocationFailed: OSStatus { get }
var kCMTimebaseError_TimerIntervalTooShort: OSStatus { get }
var kCMTimebaseError_ReadOnly: OSStatus { get }
var kCMSyncError_MissingRequiredParameter: OSStatus { get }
var kCMSyncError_InvalidParameter: OSStatus { get }
var kCMSyncError_AllocationFailed: OSStatus { get }
var kCMSyncError_RateMustBeNonZero: OSStatus { get }
@available(iOS 6.0, *)
@discardableResult
func CMClockGetTypeID() -> CFTypeID
@available(iOS 6.0, *)
@discardableResult
func CMClockGetHostTimeClock() -> CMClock
@available(iOS 6.0, *)
@discardableResult
func CMClockConvertHostTimeToSystemUnits(_ hostTime: CMTime) -> UInt64
@available(iOS 6.0, *)
@discardableResult
func CMClockMakeHostTimeFromSystemUnits(_ hostTime: UInt64) -> CMTime
@available(iOS 6.0, *)
@discardableResult
func CMClockGetTime(_ clock: CMClock) -> CMTime
@available(iOS 6.0, *)
@discardableResult
func CMClockGetAnchorTime(_ clock: CMClock, _ outClockTime: UnsafeMutablePointer<CMTime>, _ outReferenceClockTime: UnsafeMutablePointer<CMTime>) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func CMClockMightDrift(_ clock: CMClock, _ otherClock: CMClock) -> Bool
@available(iOS 6.0, *)
func CMClockInvalidate(_ clock: CMClock)
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseGetTypeID() -> CFTypeID
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseCreateWithMasterClock(_ allocator: CFAllocator?, _ masterClock: CMClock, _ timebaseOut: UnsafeMutablePointer<CMTimebase?>) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseCreateWithMasterTimebase(_ allocator: CFAllocator?, _ masterTimebase: CMTimebase, _ timebaseOut: UnsafeMutablePointer<CMTimebase?>) -> OSStatus
@available(iOS 9.0, *)
@discardableResult
func CMTimebaseCopyMasterTimebase(_ timebase: CMTimebase) -> CMTimebase?
@available(iOS 9.0, *)
@discardableResult
func CMTimebaseCopyMasterClock(_ timebase: CMTimebase) -> CMClock?
@available(iOS 9.0, *)
@discardableResult
func CMTimebaseCopyMaster(_ timebase: CMTimebase) -> CMClockOrTimebase?
@available(iOS 9.0, *)
@discardableResult
func CMTimebaseCopyUltimateMasterClock(_ timebase: CMTimebase) -> CMClock?
@available(iOS, introduced: 6.0, deprecated: 9.0)
@discardableResult
func CMTimebaseGetMasterTimebase(_ timebase: CMTimebase) -> CMTimebase?
@available(iOS, introduced: 6.0, deprecated: 9.0)
@discardableResult
func CMTimebaseGetMasterClock(_ timebase: CMTimebase) -> CMClock?
@available(iOS, introduced: 6.0, deprecated: 9.0)
@discardableResult
func CMTimebaseGetMaster(_ timebase: CMTimebase) -> CMClockOrTimebase?
@available(iOS, introduced: 6.0, deprecated: 9.0)
@discardableResult
func CMTimebaseGetUltimateMasterClock(_ timebase: CMTimebase) -> CMClock?
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseGetTime(_ timebase: CMTimebase) -> CMTime
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseGetTimeWithTimeScale(_ timebase: CMTimebase, _ timescale: CMTimeScale, _ method: CMTimeRoundingMethod) -> CMTime
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseSetTime(_ timebase: CMTimebase, _ time: CMTime) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseSetAnchorTime(_ timebase: CMTimebase, _ timebaseTime: CMTime, _ immediateMasterTime: CMTime) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseGetRate(_ timebase: CMTimebase) -> Float64
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseGetTimeAndRate(_ timebase: CMTimebase, _ outTime: UnsafeMutablePointer<CMTime>, _ outRate: UnsafeMutablePointer<Float64>) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseSetRate(_ timebase: CMTimebase, _ rate: Float64) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseSetRateAndAnchorTime(_ timebase: CMTimebase, _ rate: Float64, _ timebaseTime: CMTime, _ immediateMasterTime: CMTime) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseGetEffectiveRate(_ timebase: CMTimebase) -> Float64
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseAddTimer(_ timebase: CMTimebase, _ timer: CFRunLoopTimer, _ runloop: CFRunLoop) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseRemoveTimer(_ timebase: CMTimebase, _ timer: CFRunLoopTimer) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseSetTimerNextFireTime(_ timebase: CMTimebase, _ timer: CFRunLoopTimer, _ fireTime: CMTime, _ flags: UInt32) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseSetTimerToFireImmediately(_ timebase: CMTimebase, _ timer: CFRunLoopTimer) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseAddTimerDispatchSource(_ timebase: CMTimebase, _ timerSource: dispatch_source_t) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseRemoveTimerDispatchSource(_ timebase: CMTimebase, _ timerSource: dispatch_source_t) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseSetTimerDispatchSourceNextFireTime(_ timebase: CMTimebase, _ timerSource: dispatch_source_t, _ fireTime: CMTime, _ flags: UInt32) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseSetTimerDispatchSourceToFireImmediately(_ timebase: CMTimebase, _ timerSource: dispatch_source_t) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func CMSyncGetRelativeRate(_ ofClockOrTimebase: CMClockOrTimebase, _ relativeToClockOrTimebase: CMClockOrTimebase) -> Float64
@available(iOS 6.0, *)
@discardableResult
func CMSyncGetRelativeRateAndAnchorTime(_ ofClockOrTimebase: CMClockOrTimebase, _ relativeToClockOrTimebase: CMClockOrTimebase, _ outRelativeRate: UnsafeMutablePointer<Float64>, _ outOfClockOrTimebaseAnchorTime: UnsafeMutablePointer<CMTime>, _ outRelativeToClockOrTimebaseAnchorTime: UnsafeMutablePointer<CMTime>) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func CMSyncConvertTime(_ time: CMTime, _ fromClockOrTimebase: CMClockOrTimebase, _ toClockOrTimebase: CMClockOrTimebase) -> CMTime
@available(iOS 6.0, *)
@discardableResult
func CMSyncMightDrift(_ clockOrTimebase1: CMClockOrTimebase, _ clockOrTimebase2: CMClockOrTimebase) -> Bool
@available(iOS 6.0, *)
@discardableResult
func CMSyncGetTime(_ clockOrTimebase: CMClockOrTimebase) -> CMTime
@available(iOS 6.0, *)
@discardableResult
func CMTimebaseNotificationBarrier(_ timebase: CMTimebase) -> OSStatus
@available(iOS 6.0, *)
let kCMTimebaseNotification_EffectiveRateChanged: CFString
@available(iOS 6.0, *)
let kCMTimebaseNotification_TimeJumped: CFString
@available(iOS 7.0, *)
let kCMTimebaseNotificationKey_EventTime: CFString
