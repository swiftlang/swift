
class CFHost {
}
@available(OSX 10.3, *)
let kCFStreamErrorDomainNetDB: Int32
@available(OSX 10.3, *)
let kCFStreamErrorDomainSystemConfiguration: Int32
enum CFHostInfoType : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case addresses
  case names
  case reachability
}
struct CFHostClientContext {
  var version: CFIndex
  var info: UnsafeMutablePointer<Void>?
  var retain: CFAllocatorRetainCallBack?
  var release: CFAllocatorReleaseCallBack?
  var copyDescription: CFAllocatorCopyDescriptionCallBack
}
typealias CFHostClientCallBack = @convention(c) (CFHost, CFHostInfoType, UnsafePointer<CFStreamError>?, UnsafeMutablePointer<Void>?) -> Void
@available(OSX 10.3, *)
@discardableResult
func CFHostGetTypeID() -> CFTypeID
@available(OSX 10.3, *)
@discardableResult
func CFHostCreateWithName(_ allocator: CFAllocator?, _ hostname: CFString) -> Unmanaged<CFHost>
@available(OSX 10.3, *)
@discardableResult
func CFHostCreateWithAddress(_ allocator: CFAllocator?, _ addr: CFData) -> Unmanaged<CFHost>
@available(OSX 10.3, *)
@discardableResult
func CFHostCreateCopy(_ alloc: CFAllocator?, _ host: CFHost) -> Unmanaged<CFHost>
@available(OSX 10.3, *)
@discardableResult
func CFHostStartInfoResolution(_ theHost: CFHost, _ info: CFHostInfoType, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(OSX 10.3, *)
@discardableResult
func CFHostGetAddressing(_ theHost: CFHost, _ hasBeenResolved: UnsafeMutablePointer<DarwinBoolean>?) -> Unmanaged<CFArray>?
@available(OSX 10.3, *)
@discardableResult
func CFHostGetNames(_ theHost: CFHost, _ hasBeenResolved: UnsafeMutablePointer<DarwinBoolean>?) -> Unmanaged<CFArray>?
@available(OSX 10.3, *)
@discardableResult
func CFHostGetReachability(_ theHost: CFHost, _ hasBeenResolved: UnsafeMutablePointer<DarwinBoolean>?) -> Unmanaged<CFData>?
@available(OSX 10.3, *)
func CFHostCancelInfoResolution(_ theHost: CFHost, _ info: CFHostInfoType)
@available(OSX 10.3, *)
@discardableResult
func CFHostSetClient(_ theHost: CFHost, _ clientCB: CFHostClientCallBack?, _ clientContext: UnsafeMutablePointer<CFHostClientContext>?) -> Bool
@available(OSX 10.3, *)
func CFHostScheduleWithRunLoop(_ theHost: CFHost, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
@available(OSX 10.3, *)
func CFHostUnscheduleFromRunLoop(_ theHost: CFHost, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
