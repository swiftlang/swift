
class CFRunLoop {
}
class CFRunLoopSource {
}
class CFRunLoopObserver {
}
class CFRunLoopTimer {
}
enum CFRunLoopRunResult : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case finished
  case stopped
  case timedOut
  case handledSource
}
struct CFRunLoopActivity : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  static var entry: CFRunLoopActivity { get }
  static var beforeTimers: CFRunLoopActivity { get }
  static var beforeSources: CFRunLoopActivity { get }
  static var beforeWaiting: CFRunLoopActivity { get }
  static var afterWaiting: CFRunLoopActivity { get }
  static var exit: CFRunLoopActivity { get }
  static var allActivities: CFRunLoopActivity { get }
}
let kCFRunLoopDefaultMode: CFString!
let kCFRunLoopCommonModes: CFString!
@discardableResult
func CFRunLoopGetTypeID() -> CFTypeID
@discardableResult
func CFRunLoopGetCurrent() -> CFRunLoop!
@discardableResult
func CFRunLoopGetMain() -> CFRunLoop!
@discardableResult
func CFRunLoopCopyCurrentMode(_ rl: CFRunLoop!) -> CFString!
@discardableResult
func CFRunLoopCopyAllModes(_ rl: CFRunLoop!) -> CFArray!
func CFRunLoopAddCommonMode(_ rl: CFRunLoop!, _ mode: CFString!)
@discardableResult
func CFRunLoopGetNextTimerFireDate(_ rl: CFRunLoop!, _ mode: CFString!) -> CFAbsoluteTime
func CFRunLoopRun()
@discardableResult
func CFRunLoopRunInMode(_ mode: CFString!, _ seconds: CFTimeInterval, _ returnAfterSourceHandled: Bool) -> CFRunLoopRunResult
@discardableResult
func CFRunLoopIsWaiting(_ rl: CFRunLoop!) -> Bool
func CFRunLoopWakeUp(_ rl: CFRunLoop!)
func CFRunLoopStop(_ rl: CFRunLoop!)
@available(tvOS 4.0, *)
func CFRunLoopPerformBlock(_ rl: CFRunLoop!, _ mode: CFTypeRef!, _ block: (() -> Void)!)
@discardableResult
func CFRunLoopContainsSource(_ rl: CFRunLoop!, _ source: CFRunLoopSource!, _ mode: CFString!) -> Bool
func CFRunLoopAddSource(_ rl: CFRunLoop!, _ source: CFRunLoopSource!, _ mode: CFString!)
func CFRunLoopRemoveSource(_ rl: CFRunLoop!, _ source: CFRunLoopSource!, _ mode: CFString!)
@discardableResult
func CFRunLoopContainsObserver(_ rl: CFRunLoop!, _ observer: CFRunLoopObserver!, _ mode: CFString!) -> Bool
func CFRunLoopAddObserver(_ rl: CFRunLoop!, _ observer: CFRunLoopObserver!, _ mode: CFString!)
func CFRunLoopRemoveObserver(_ rl: CFRunLoop!, _ observer: CFRunLoopObserver!, _ mode: CFString!)
@discardableResult
func CFRunLoopContainsTimer(_ rl: CFRunLoop!, _ timer: CFRunLoopTimer!, _ mode: CFString!) -> Bool
func CFRunLoopAddTimer(_ rl: CFRunLoop!, _ timer: CFRunLoopTimer!, _ mode: CFString!)
func CFRunLoopRemoveTimer(_ rl: CFRunLoop!, _ timer: CFRunLoopTimer!, _ mode: CFString!)
struct CFRunLoopSourceContext {
  var version: CFIndex
  var info: UnsafeMutablePointer<Void>!
  var retain: (@convention(c) (UnsafePointer<Void>!) -> UnsafePointer<Void>!)!
  var release: (@convention(c) (UnsafePointer<Void>!) -> Void)!
  var copyDescription: (@convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!)!
  var equal: (@convention(c) (UnsafePointer<Void>!, UnsafePointer<Void>!) -> DarwinBoolean)!
  var hash: (@convention(c) (UnsafePointer<Void>!) -> CFHashCode)!
  var schedule: (@convention(c) (UnsafeMutablePointer<Void>!, CFRunLoop!, CFString!) -> Void)!
  var cancel: (@convention(c) (UnsafeMutablePointer<Void>!, CFRunLoop!, CFString!) -> Void)!
  var perform: (@convention(c) (UnsafeMutablePointer<Void>!) -> Void)!
  init()
  init(version version: CFIndex, info info: UnsafeMutablePointer<Void>!, retain retain: (@convention(c) (UnsafePointer<Void>!) -> UnsafePointer<Void>!)!, release release: (@convention(c) (UnsafePointer<Void>!) -> Void)!, copyDescription copyDescription: (@convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!)!, equal equal: (@convention(c) (UnsafePointer<Void>!, UnsafePointer<Void>!) -> DarwinBoolean)!, hash hash: (@convention(c) (UnsafePointer<Void>!) -> CFHashCode)!, schedule schedule: (@convention(c) (UnsafeMutablePointer<Void>!, CFRunLoop!, CFString!) -> Void)!, cancel cancel: (@convention(c) (UnsafeMutablePointer<Void>!, CFRunLoop!, CFString!) -> Void)!, perform perform: (@convention(c) (UnsafeMutablePointer<Void>!) -> Void)!)
}
struct CFRunLoopSourceContext1 {
  var version: CFIndex
  var info: UnsafeMutablePointer<Void>!
  var retain: (@convention(c) (UnsafePointer<Void>!) -> UnsafePointer<Void>!)!
  var release: (@convention(c) (UnsafePointer<Void>!) -> Void)!
  var copyDescription: (@convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!)!
  var equal: (@convention(c) (UnsafePointer<Void>!, UnsafePointer<Void>!) -> DarwinBoolean)!
  var hash: (@convention(c) (UnsafePointer<Void>!) -> CFHashCode)!
  var getPort: (@convention(c) (UnsafeMutablePointer<Void>!) -> mach_port_t)!
  var perform: (@convention(c) (UnsafeMutablePointer<Void>!, CFIndex, CFAllocator!, UnsafeMutablePointer<Void>!) -> UnsafeMutablePointer<Void>!)!
  init()
  init(version version: CFIndex, info info: UnsafeMutablePointer<Void>!, retain retain: (@convention(c) (UnsafePointer<Void>!) -> UnsafePointer<Void>!)!, release release: (@convention(c) (UnsafePointer<Void>!) -> Void)!, copyDescription copyDescription: (@convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!)!, equal equal: (@convention(c) (UnsafePointer<Void>!, UnsafePointer<Void>!) -> DarwinBoolean)!, hash hash: (@convention(c) (UnsafePointer<Void>!) -> CFHashCode)!, getPort getPort: (@convention(c) (UnsafeMutablePointer<Void>!) -> mach_port_t)!, perform perform: (@convention(c) (UnsafeMutablePointer<Void>!, CFIndex, CFAllocator!, UnsafeMutablePointer<Void>!) -> UnsafeMutablePointer<Void>!)!)
}
@discardableResult
func CFRunLoopSourceGetTypeID() -> CFTypeID
@discardableResult
func CFRunLoopSourceCreate(_ allocator: CFAllocator!, _ order: CFIndex, _ context: UnsafeMutablePointer<CFRunLoopSourceContext>!) -> CFRunLoopSource!
@discardableResult
func CFRunLoopSourceGetOrder(_ source: CFRunLoopSource!) -> CFIndex
func CFRunLoopSourceInvalidate(_ source: CFRunLoopSource!)
@discardableResult
func CFRunLoopSourceIsValid(_ source: CFRunLoopSource!) -> Bool
func CFRunLoopSourceGetContext(_ source: CFRunLoopSource!, _ context: UnsafeMutablePointer<CFRunLoopSourceContext>!)
func CFRunLoopSourceSignal(_ source: CFRunLoopSource!)
struct CFRunLoopObserverContext {
  var version: CFIndex
  var info: UnsafeMutablePointer<Void>!
  var retain: (@convention(c) (UnsafePointer<Void>!) -> UnsafePointer<Void>!)!
  var release: (@convention(c) (UnsafePointer<Void>!) -> Void)!
  var copyDescription: (@convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!)!
  init()
  init(version version: CFIndex, info info: UnsafeMutablePointer<Void>!, retain retain: (@convention(c) (UnsafePointer<Void>!) -> UnsafePointer<Void>!)!, release release: (@convention(c) (UnsafePointer<Void>!) -> Void)!, copyDescription copyDescription: (@convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!)!)
}
typealias CFRunLoopObserverCallBack = @convention(c) (CFRunLoopObserver!, CFRunLoopActivity, UnsafeMutablePointer<Void>!) -> Void
@discardableResult
func CFRunLoopObserverGetTypeID() -> CFTypeID
@discardableResult
func CFRunLoopObserverCreate(_ allocator: CFAllocator!, _ activities: CFOptionFlags, _ repeats: Bool, _ order: CFIndex, _ callout: CFRunLoopObserverCallBack!, _ context: UnsafeMutablePointer<CFRunLoopObserverContext>!) -> CFRunLoopObserver!
@available(tvOS 5.0, *)
@discardableResult
func CFRunLoopObserverCreateWithHandler(_ allocator: CFAllocator!, _ activities: CFOptionFlags, _ repeats: Bool, _ order: CFIndex, _ block: ((CFRunLoopObserver!, CFRunLoopActivity) -> Void)!) -> CFRunLoopObserver!
@discardableResult
func CFRunLoopObserverGetActivities(_ observer: CFRunLoopObserver!) -> CFOptionFlags
@discardableResult
func CFRunLoopObserverDoesRepeat(_ observer: CFRunLoopObserver!) -> Bool
@discardableResult
func CFRunLoopObserverGetOrder(_ observer: CFRunLoopObserver!) -> CFIndex
func CFRunLoopObserverInvalidate(_ observer: CFRunLoopObserver!)
@discardableResult
func CFRunLoopObserverIsValid(_ observer: CFRunLoopObserver!) -> Bool
func CFRunLoopObserverGetContext(_ observer: CFRunLoopObserver!, _ context: UnsafeMutablePointer<CFRunLoopObserverContext>!)
struct CFRunLoopTimerContext {
  var version: CFIndex
  var info: UnsafeMutablePointer<Void>!
  var retain: (@convention(c) (UnsafePointer<Void>!) -> UnsafePointer<Void>!)!
  var release: (@convention(c) (UnsafePointer<Void>!) -> Void)!
  var copyDescription: (@convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!)!
  init()
  init(version version: CFIndex, info info: UnsafeMutablePointer<Void>!, retain retain: (@convention(c) (UnsafePointer<Void>!) -> UnsafePointer<Void>!)!, release release: (@convention(c) (UnsafePointer<Void>!) -> Void)!, copyDescription copyDescription: (@convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!)!)
}
typealias CFRunLoopTimerCallBack = @convention(c) (CFRunLoopTimer!, UnsafeMutablePointer<Void>!) -> Void
@discardableResult
func CFRunLoopTimerGetTypeID() -> CFTypeID
@discardableResult
func CFRunLoopTimerCreate(_ allocator: CFAllocator!, _ fireDate: CFAbsoluteTime, _ interval: CFTimeInterval, _ flags: CFOptionFlags, _ order: CFIndex, _ callout: CFRunLoopTimerCallBack!, _ context: UnsafeMutablePointer<CFRunLoopTimerContext>!) -> CFRunLoopTimer!
@available(tvOS 5.0, *)
@discardableResult
func CFRunLoopTimerCreateWithHandler(_ allocator: CFAllocator!, _ fireDate: CFAbsoluteTime, _ interval: CFTimeInterval, _ flags: CFOptionFlags, _ order: CFIndex, _ block: ((CFRunLoopTimer!) -> Void)!) -> CFRunLoopTimer!
@discardableResult
func CFRunLoopTimerGetNextFireDate(_ timer: CFRunLoopTimer!) -> CFAbsoluteTime
func CFRunLoopTimerSetNextFireDate(_ timer: CFRunLoopTimer!, _ fireDate: CFAbsoluteTime)
@discardableResult
func CFRunLoopTimerGetInterval(_ timer: CFRunLoopTimer!) -> CFTimeInterval
@discardableResult
func CFRunLoopTimerDoesRepeat(_ timer: CFRunLoopTimer!) -> Bool
@discardableResult
func CFRunLoopTimerGetOrder(_ timer: CFRunLoopTimer!) -> CFIndex
func CFRunLoopTimerInvalidate(_ timer: CFRunLoopTimer!)
@discardableResult
func CFRunLoopTimerIsValid(_ timer: CFRunLoopTimer!) -> Bool
func CFRunLoopTimerGetContext(_ timer: CFRunLoopTimer!, _ context: UnsafeMutablePointer<CFRunLoopTimerContext>!)
@available(tvOS 7.0, *)
@discardableResult
func CFRunLoopTimerGetTolerance(_ timer: CFRunLoopTimer!) -> CFTimeInterval
@available(tvOS 7.0, *)
func CFRunLoopTimerSetTolerance(_ timer: CFRunLoopTimer!, _ tolerance: CFTimeInterval)
