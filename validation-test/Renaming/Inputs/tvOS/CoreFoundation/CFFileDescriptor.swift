
typealias CFFileDescriptorNativeDescriptor = Int32
class CFFileDescriptor {
}
var kCFFileDescriptorReadCallBack: CFOptionFlags { get }
var kCFFileDescriptorWriteCallBack: CFOptionFlags { get }
typealias CFFileDescriptorCallBack = @convention(c) (CFFileDescriptor!, CFOptionFlags, UnsafeMutablePointer<Void>!) -> Void
struct CFFileDescriptorContext {
  var version: CFIndex
  var info: UnsafeMutablePointer<Void>!
  var retain: (@convention(c) (UnsafeMutablePointer<Void>!) -> UnsafeMutablePointer<Void>!)!
  var release: (@convention(c) (UnsafeMutablePointer<Void>!) -> Void)!
  var copyDescription: (@convention(c) (UnsafeMutablePointer<Void>!) -> Unmanaged<CFString>!)!
  init()
  init(version version: CFIndex, info info: UnsafeMutablePointer<Void>!, retain retain: (@convention(c) (UnsafeMutablePointer<Void>!) -> UnsafeMutablePointer<Void>!)!, release release: (@convention(c) (UnsafeMutablePointer<Void>!) -> Void)!, copyDescription copyDescription: (@convention(c) (UnsafeMutablePointer<Void>!) -> Unmanaged<CFString>!)!)
}
@available(tvOS 2.0, *)
@discardableResult
func CFFileDescriptorGetTypeID() -> CFTypeID
@available(tvOS 2.0, *)
@discardableResult
func CFFileDescriptorCreate(_ allocator: CFAllocator!, _ fd: CFFileDescriptorNativeDescriptor, _ closeOnInvalidate: Bool, _ callout: CFFileDescriptorCallBack!, _ context: UnsafePointer<CFFileDescriptorContext>!) -> CFFileDescriptor!
@available(tvOS 2.0, *)
@discardableResult
func CFFileDescriptorGetNativeDescriptor(_ f: CFFileDescriptor!) -> CFFileDescriptorNativeDescriptor
@available(tvOS 2.0, *)
func CFFileDescriptorGetContext(_ f: CFFileDescriptor!, _ context: UnsafeMutablePointer<CFFileDescriptorContext>!)
@available(tvOS 2.0, *)
func CFFileDescriptorEnableCallBacks(_ f: CFFileDescriptor!, _ callBackTypes: CFOptionFlags)
@available(tvOS 2.0, *)
func CFFileDescriptorDisableCallBacks(_ f: CFFileDescriptor!, _ callBackTypes: CFOptionFlags)
@available(tvOS 2.0, *)
func CFFileDescriptorInvalidate(_ f: CFFileDescriptor!)
@available(tvOS 2.0, *)
@discardableResult
func CFFileDescriptorIsValid(_ f: CFFileDescriptor!) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CFFileDescriptorCreateRunLoopSource(_ allocator: CFAllocator!, _ f: CFFileDescriptor!, _ order: CFIndex) -> CFRunLoopSource!
