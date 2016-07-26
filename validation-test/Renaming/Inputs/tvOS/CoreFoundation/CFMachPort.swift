
class CFMachPort {
}
struct CFMachPortContext {
  var version: CFIndex
  var info: UnsafeMutablePointer<Void>!
  var retain: (@convention(c) (UnsafePointer<Void>!) -> UnsafePointer<Void>!)!
  var release: (@convention(c) (UnsafePointer<Void>!) -> Void)!
  var copyDescription: (@convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!)!
  init()
  init(version version: CFIndex, info info: UnsafeMutablePointer<Void>!, retain retain: (@convention(c) (UnsafePointer<Void>!) -> UnsafePointer<Void>!)!, release release: (@convention(c) (UnsafePointer<Void>!) -> Void)!, copyDescription copyDescription: (@convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!)!)
}
typealias CFMachPortCallBack = @convention(c) (CFMachPort!, UnsafeMutablePointer<Void>!, CFIndex, UnsafeMutablePointer<Void>!) -> Void
typealias CFMachPortInvalidationCallBack = @convention(c) (CFMachPort!, UnsafeMutablePointer<Void>!) -> Void
@discardableResult
func CFMachPortGetTypeID() -> CFTypeID
@discardableResult
func CFMachPortCreate(_ allocator: CFAllocator!, _ callout: CFMachPortCallBack!, _ context: UnsafeMutablePointer<CFMachPortContext>!, _ shouldFreeInfo: UnsafeMutablePointer<DarwinBoolean>!) -> CFMachPort!
@discardableResult
func CFMachPortCreateWithPort(_ allocator: CFAllocator!, _ portNum: mach_port_t, _ callout: CFMachPortCallBack!, _ context: UnsafeMutablePointer<CFMachPortContext>!, _ shouldFreeInfo: UnsafeMutablePointer<DarwinBoolean>!) -> CFMachPort!
@discardableResult
func CFMachPortGetPort(_ port: CFMachPort!) -> mach_port_t
func CFMachPortGetContext(_ port: CFMachPort!, _ context: UnsafeMutablePointer<CFMachPortContext>!)
func CFMachPortInvalidate(_ port: CFMachPort!)
@discardableResult
func CFMachPortIsValid(_ port: CFMachPort!) -> Bool
@discardableResult
func CFMachPortGetInvalidationCallBack(_ port: CFMachPort!) -> CFMachPortInvalidationCallBack!
func CFMachPortSetInvalidationCallBack(_ port: CFMachPort!, _ callout: CFMachPortInvalidationCallBack!)
@discardableResult
func CFMachPortCreateRunLoopSource(_ allocator: CFAllocator!, _ port: CFMachPort!, _ order: CFIndex) -> CFRunLoopSource!
