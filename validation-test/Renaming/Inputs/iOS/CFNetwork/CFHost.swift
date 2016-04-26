
class CFHost {
}
@available(iOS 2.0, *)
let kCFStreamErrorDomainNetDB: Int32
@available(iOS 2.0, *)
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
@available(iOS 2.0, *)
@discardableResult
func CFHostGetTypeID() -> CFTypeID
@available(iOS 2.0, *)
@discardableResult
func CFHostCreateWithName(_ allocator: CFAllocator?, _ hostname: CFString) -> Unmanaged<CFHost>
@available(iOS 2.0, *)
@discardableResult
func CFHostCreateWithAddress(_ allocator: CFAllocator?, _ addr: CFData) -> Unmanaged<CFHost>
@available(iOS 2.0, *)
@discardableResult
func CFHostCreateCopy(_ alloc: CFAllocator?, _ host: CFHost) -> Unmanaged<CFHost>
@available(iOS 2.0, *)
@discardableResult
func CFHostStartInfoResolution(_ theHost: CFHost, _ info: CFHostInfoType, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(iOS 2.0, *)
@discardableResult
func CFHostGetAddressing(_ theHost: CFHost, _ hasBeenResolved: UnsafeMutablePointer<DarwinBoolean>?) -> Unmanaged<CFArray>?
@available(iOS 2.0, *)
@discardableResult
func CFHostGetNames(_ theHost: CFHost, _ hasBeenResolved: UnsafeMutablePointer<DarwinBoolean>?) -> Unmanaged<CFArray>?
@available(iOS 2.0, *)
@discardableResult
func CFHostGetReachability(_ theHost: CFHost, _ hasBeenResolved: UnsafeMutablePointer<DarwinBoolean>?) -> Unmanaged<CFData>?
@available(iOS 2.0, *)
func CFHostCancelInfoResolution(_ theHost: CFHost, _ info: CFHostInfoType)
@available(iOS 2.0, *)
@discardableResult
func CFHostSetClient(_ theHost: CFHost, _ clientCB: CFHostClientCallBack?, _ clientContext: UnsafeMutablePointer<CFHostClientContext>?) -> Bool
@available(iOS 2.0, *)
func CFHostScheduleWithRunLoop(_ theHost: CFHost, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
@available(iOS 2.0, *)
func CFHostUnscheduleFromRunLoop(_ theHost: CFHost, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
