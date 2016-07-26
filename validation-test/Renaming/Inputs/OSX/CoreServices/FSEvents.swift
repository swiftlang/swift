
typealias FSEventStreamCreateFlags = UInt32
var kFSEventStreamCreateFlagNone: Int { get }
var kFSEventStreamCreateFlagUseCFTypes: Int { get }
var kFSEventStreamCreateFlagNoDefer: Int { get }
var kFSEventStreamCreateFlagWatchRoot: Int { get }
@available(OSX 10.6, *)
var kFSEventStreamCreateFlagIgnoreSelf: Int { get }
@available(OSX 10.7, *)
var kFSEventStreamCreateFlagFileEvents: Int { get }
@available(OSX 10.9, *)
var kFSEventStreamCreateFlagMarkSelf: Int { get }
typealias FSEventStreamEventFlags = UInt32
var kFSEventStreamEventFlagNone: Int { get }
var kFSEventStreamEventFlagMustScanSubDirs: Int { get }
var kFSEventStreamEventFlagUserDropped: Int { get }
var kFSEventStreamEventFlagKernelDropped: Int { get }
var kFSEventStreamEventFlagEventIdsWrapped: Int { get }
var kFSEventStreamEventFlagHistoryDone: Int { get }
var kFSEventStreamEventFlagRootChanged: Int { get }
var kFSEventStreamEventFlagMount: Int { get }
var kFSEventStreamEventFlagUnmount: Int { get }
@available(OSX 10.7, *)
var kFSEventStreamEventFlagItemCreated: Int { get }
@available(OSX 10.7, *)
var kFSEventStreamEventFlagItemRemoved: Int { get }
@available(OSX 10.7, *)
var kFSEventStreamEventFlagItemInodeMetaMod: Int { get }
@available(OSX 10.7, *)
var kFSEventStreamEventFlagItemRenamed: Int { get }
@available(OSX 10.7, *)
var kFSEventStreamEventFlagItemModified: Int { get }
@available(OSX 10.7, *)
var kFSEventStreamEventFlagItemFinderInfoMod: Int { get }
@available(OSX 10.7, *)
var kFSEventStreamEventFlagItemChangeOwner: Int { get }
@available(OSX 10.7, *)
var kFSEventStreamEventFlagItemXattrMod: Int { get }
@available(OSX 10.7, *)
var kFSEventStreamEventFlagItemIsFile: Int { get }
@available(OSX 10.7, *)
var kFSEventStreamEventFlagItemIsDir: Int { get }
@available(OSX 10.7, *)
var kFSEventStreamEventFlagItemIsSymlink: Int { get }
@available(OSX 10.9, *)
var kFSEventStreamEventFlagOwnEvent: Int { get }
@available(OSX 10.10, *)
var kFSEventStreamEventFlagItemIsHardlink: Int { get }
@available(OSX 10.10, *)
var kFSEventStreamEventFlagItemIsLastHardlink: Int { get }
typealias FSEventStreamEventId = UInt64
var kFSEventStreamEventIdSinceNow: UInt { get }
typealias FSEventStreamRef = OpaquePointer
typealias ConstFSEventStreamRef = OpaquePointer
struct FSEventStreamContext {
  var version: CFIndex
  var info: UnsafeMutablePointer<Void>?
  var retain: CFAllocatorRetainCallBack?
  var release: CFAllocatorReleaseCallBack?
  var copyDescription: CFAllocatorCopyDescriptionCallBack?
  init()
  init(version version: CFIndex, info info: UnsafeMutablePointer<Void>?, retain retain: CFAllocatorRetainCallBack?, release release: CFAllocatorReleaseCallBack?, copyDescription copyDescription: CFAllocatorCopyDescriptionCallBack?)
}
typealias FSEventStreamCallback = @convention(c) (ConstFSEventStreamRef, UnsafeMutablePointer<Void>?, Int, UnsafeMutablePointer<Void>, UnsafePointer<FSEventStreamEventFlags>!, UnsafePointer<FSEventStreamEventId>!) -> Void
@available(OSX 10.5, *)
@discardableResult
func FSEventStreamCreate(_ allocator: CFAllocator?, _ callback: FSEventStreamCallback, _ context: UnsafeMutablePointer<FSEventStreamContext>?, _ pathsToWatch: CFArray, _ sinceWhen: FSEventStreamEventId, _ latency: CFTimeInterval, _ flags: FSEventStreamCreateFlags) -> FSEventStreamRef?
@available(OSX 10.5, *)
@discardableResult
func FSEventStreamCreateRelativeToDevice(_ allocator: CFAllocator?, _ callback: FSEventStreamCallback, _ context: UnsafeMutablePointer<FSEventStreamContext>?, _ deviceToWatch: dev_t, _ pathsToWatchRelativeToDevice: CFArray, _ sinceWhen: FSEventStreamEventId, _ latency: CFTimeInterval, _ flags: FSEventStreamCreateFlags) -> FSEventStreamRef?
@available(OSX 10.5, *)
@discardableResult
func FSEventStreamGetLatestEventId(_ streamRef: ConstFSEventStreamRef) -> FSEventStreamEventId
@available(OSX 10.5, *)
@discardableResult
func FSEventStreamGetDeviceBeingWatched(_ streamRef: ConstFSEventStreamRef) -> dev_t
@available(OSX 10.5, *)
@discardableResult
func FSEventStreamCopyPathsBeingWatched(_ streamRef: ConstFSEventStreamRef) -> CFArray
@available(OSX 10.5, *)
@discardableResult
func FSEventsGetCurrentEventId() -> FSEventStreamEventId
@available(OSX 10.5, *)
@discardableResult
func FSEventsCopyUUIDForDevice(_ dev: dev_t) -> CFUUID?
@available(OSX 10.5, *)
@discardableResult
func FSEventsGetLastEventIdForDeviceBeforeTime(_ dev: dev_t, _ time: CFAbsoluteTime) -> FSEventStreamEventId
@available(OSX 10.5, *)
@discardableResult
func FSEventsPurgeEventsForDeviceUpToEventId(_ dev: dev_t, _ eventId: FSEventStreamEventId) -> Bool
@available(OSX 10.5, *)
func FSEventStreamRetain(_ streamRef: FSEventStreamRef)
@available(OSX 10.5, *)
func FSEventStreamRelease(_ streamRef: FSEventStreamRef)
@available(OSX 10.5, *)
func FSEventStreamScheduleWithRunLoop(_ streamRef: FSEventStreamRef, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
@available(OSX 10.5, *)
func FSEventStreamUnscheduleFromRunLoop(_ streamRef: FSEventStreamRef, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
@available(OSX 10.6, *)
func FSEventStreamSetDispatchQueue(_ streamRef: FSEventStreamRef, _ q: dispatch_queue_t?)
@available(OSX 10.5, *)
func FSEventStreamInvalidate(_ streamRef: FSEventStreamRef)
@available(OSX 10.5, *)
@discardableResult
func FSEventStreamStart(_ streamRef: FSEventStreamRef) -> Bool
@available(OSX 10.5, *)
@discardableResult
func FSEventStreamFlushAsync(_ streamRef: FSEventStreamRef) -> FSEventStreamEventId
@available(OSX 10.5, *)
func FSEventStreamFlushSync(_ streamRef: FSEventStreamRef)
@available(OSX 10.5, *)
func FSEventStreamStop(_ streamRef: FSEventStreamRef)
@available(OSX 10.5, *)
func FSEventStreamShow(_ streamRef: ConstFSEventStreamRef)
@available(OSX 10.5, *)
@discardableResult
func FSEventStreamCopyDescription(_ streamRef: ConstFSEventStreamRef) -> CFString
@available(OSX 10.9, *)
@discardableResult
func FSEventStreamSetExclusionPaths(_ streamRef: FSEventStreamRef, _ pathsToExclude: CFArray) -> Bool
