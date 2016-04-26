
class CFMessagePort {
}
var kCFMessagePortSuccess: Int32 { get }
var kCFMessagePortSendTimeout: Int32 { get }
var kCFMessagePortReceiveTimeout: Int32 { get }
var kCFMessagePortIsInvalid: Int32 { get }
var kCFMessagePortTransportError: Int32 { get }
var kCFMessagePortBecameInvalidError: Int32 { get }
struct CFMessagePortContext {
  var version: CFIndex
  var info: UnsafeMutablePointer<Void>!
  var retain: (@convention(c) (UnsafePointer<Void>!) -> UnsafePointer<Void>!)!
  var release: (@convention(c) (UnsafePointer<Void>!) -> Void)!
  var copyDescription: (@convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!)!
  init()
  init(version version: CFIndex, info info: UnsafeMutablePointer<Void>!, retain retain: (@convention(c) (UnsafePointer<Void>!) -> UnsafePointer<Void>!)!, release release: (@convention(c) (UnsafePointer<Void>!) -> Void)!, copyDescription copyDescription: (@convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!)!)
}
typealias CFMessagePortCallBack = @convention(c) (CFMessagePort!, Int32, CFData!, UnsafeMutablePointer<Void>!) -> Unmanaged<CFData>!
typealias CFMessagePortInvalidationCallBack = @convention(c) (CFMessagePort!, UnsafeMutablePointer<Void>!) -> Void
@discardableResult
func CFMessagePortGetTypeID() -> CFTypeID
@discardableResult
func CFMessagePortCreateLocal(_ allocator: CFAllocator!, _ name: CFString!, _ callout: CFMessagePortCallBack!, _ context: UnsafeMutablePointer<CFMessagePortContext>!, _ shouldFreeInfo: UnsafeMutablePointer<DarwinBoolean>!) -> CFMessagePort!
@discardableResult
func CFMessagePortCreateRemote(_ allocator: CFAllocator!, _ name: CFString!) -> CFMessagePort!
@discardableResult
func CFMessagePortIsRemote(_ ms: CFMessagePort!) -> Bool
@discardableResult
func CFMessagePortGetName(_ ms: CFMessagePort!) -> CFString!
@discardableResult
func CFMessagePortSetName(_ ms: CFMessagePort!, _ newName: CFString!) -> Bool
func CFMessagePortGetContext(_ ms: CFMessagePort!, _ context: UnsafeMutablePointer<CFMessagePortContext>!)
func CFMessagePortInvalidate(_ ms: CFMessagePort!)
@discardableResult
func CFMessagePortIsValid(_ ms: CFMessagePort!) -> Bool
@discardableResult
func CFMessagePortGetInvalidationCallBack(_ ms: CFMessagePort!) -> CFMessagePortInvalidationCallBack!
func CFMessagePortSetInvalidationCallBack(_ ms: CFMessagePort!, _ callout: CFMessagePortInvalidationCallBack!)
@discardableResult
func CFMessagePortSendRequest(_ remote: CFMessagePort!, _ msgid: Int32, _ data: CFData!, _ sendTimeout: CFTimeInterval, _ rcvTimeout: CFTimeInterval, _ replyMode: CFString!, _ returnData: UnsafeMutablePointer<Unmanaged<CFData>?>!) -> Int32
@discardableResult
func CFMessagePortCreateRunLoopSource(_ allocator: CFAllocator!, _ local: CFMessagePort!, _ order: CFIndex) -> CFRunLoopSource!
@available(tvOS 4.0, *)
func CFMessagePortSetDispatchQueue(_ ms: CFMessagePort!, _ queue: dispatch_queue_t!)
