
class CFNetService {
}
class CFNetServiceMonitor {
}
class CFNetServiceBrowser {
}
@available(OSX 10.2, *)
let kCFStreamErrorDomainMach: Int32
@available(OSX 10.2, *)
let kCFStreamErrorDomainNetServices: Int32
enum CFNetServicesError : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case unknown
  case collision
  case notFound
  case inProgress
  case badArgument
  case cancel
  case invalid
  case timeout
}
enum CFNetServiceMonitorType : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case TXT
}
struct CFNetServiceRegisterFlags : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  static var noAutoRename: CFNetServiceRegisterFlags { get }
}
struct CFNetServiceBrowserFlags : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  static var moreComing: CFNetServiceBrowserFlags { get }
  static var isDomain: CFNetServiceBrowserFlags { get }
  static var isDefault: CFNetServiceBrowserFlags { get }
  static var remove: CFNetServiceBrowserFlags { get }
}
struct CFNetServiceClientContext {
  var version: CFIndex
  var info: UnsafeMutablePointer<Void>?
  var retain: CFAllocatorRetainCallBack?
  var release: CFAllocatorReleaseCallBack?
  var copyDescription: CFAllocatorCopyDescriptionCallBack?
  init()
  init(version version: CFIndex, info info: UnsafeMutablePointer<Void>?, retain retain: CFAllocatorRetainCallBack?, release release: CFAllocatorReleaseCallBack?, copyDescription copyDescription: CFAllocatorCopyDescriptionCallBack?)
}
typealias CFNetServiceClientCallBack = @convention(c) (CFNetService, UnsafeMutablePointer<CFStreamError>?, UnsafeMutablePointer<Void>?) -> Void
typealias CFNetServiceMonitorClientCallBack = @convention(c) (CFNetServiceMonitor, CFNetService, CFNetServiceMonitorType, CFData, UnsafeMutablePointer<CFStreamError>?, UnsafeMutablePointer<Void>?) -> Void
typealias CFNetServiceBrowserClientCallBack = @convention(c) (CFNetServiceBrowser, CFOptionFlags, CFTypeRef, UnsafeMutablePointer<CFStreamError>?, UnsafeMutablePointer<Void>?) -> Void
@available(OSX 10.2, *)
@discardableResult
func CFNetServiceGetTypeID() -> CFTypeID
@available(OSX 10.4, *)
@discardableResult
func CFNetServiceMonitorGetTypeID() -> CFTypeID
@available(OSX 10.2, *)
@discardableResult
func CFNetServiceBrowserGetTypeID() -> CFTypeID
@available(OSX 10.2, *)
@discardableResult
func CFNetServiceCreate(_ alloc: CFAllocator?, _ domain: CFString, _ serviceType: CFString, _ name: CFString, _ port: Int32) -> Unmanaged<CFNetService>
@available(OSX 10.3, *)
@discardableResult
func CFNetServiceCreateCopy(_ alloc: CFAllocator?, _ service: CFNetService) -> Unmanaged<CFNetService>
@available(OSX 10.2, *)
@discardableResult
func CFNetServiceGetDomain(_ theService: CFNetService) -> Unmanaged<CFString>
@available(OSX 10.2, *)
@discardableResult
func CFNetServiceGetType(_ theService: CFNetService) -> Unmanaged<CFString>
@available(OSX 10.2, *)
@discardableResult
func CFNetServiceGetName(_ theService: CFNetService) -> Unmanaged<CFString>
@available(OSX 10.4, *)
@discardableResult
func CFNetServiceRegisterWithOptions(_ theService: CFNetService, _ options: CFOptionFlags, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(OSX 10.4, *)
@discardableResult
func CFNetServiceResolveWithTimeout(_ theService: CFNetService, _ timeout: CFTimeInterval, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(OSX 10.2, *)
func CFNetServiceCancel(_ theService: CFNetService)
@available(OSX 10.4, *)
@discardableResult
func CFNetServiceGetTargetHost(_ theService: CFNetService) -> Unmanaged<CFString>?
@available(OSX 10.5, *)
@discardableResult
func CFNetServiceGetPortNumber(_ theService: CFNetService) -> Int32
@available(OSX 10.2, *)
@discardableResult
func CFNetServiceGetAddressing(_ theService: CFNetService) -> Unmanaged<CFArray>?
@available(OSX 10.4, *)
@discardableResult
func CFNetServiceGetTXTData(_ theService: CFNetService) -> Unmanaged<CFData>?
@available(OSX 10.4, *)
@discardableResult
func CFNetServiceSetTXTData(_ theService: CFNetService, _ txtRecord: CFData) -> Bool
@available(OSX 10.4, *)
@discardableResult
func CFNetServiceCreateDictionaryWithTXTData(_ alloc: CFAllocator?, _ txtRecord: CFData) -> Unmanaged<CFDictionary>?
@available(OSX 10.4, *)
@discardableResult
func CFNetServiceCreateTXTDataWithDictionary(_ alloc: CFAllocator?, _ keyValuePairs: CFDictionary) -> Unmanaged<CFData>?
@available(OSX 10.2, *)
@discardableResult
func CFNetServiceSetClient(_ theService: CFNetService, _ clientCB: CFNetServiceClientCallBack?, _ clientContext: UnsafeMutablePointer<CFNetServiceClientContext>?) -> Bool
@available(OSX 10.2, *)
func CFNetServiceScheduleWithRunLoop(_ theService: CFNetService, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
@available(OSX 10.2, *)
func CFNetServiceUnscheduleFromRunLoop(_ theService: CFNetService, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
@available(OSX 10.4, *)
@discardableResult
func CFNetServiceMonitorCreate(_ alloc: CFAllocator?, _ theService: CFNetService, _ clientCB: CFNetServiceMonitorClientCallBack, _ clientContext: UnsafeMutablePointer<CFNetServiceClientContext>) -> Unmanaged<CFNetServiceMonitor>
@available(OSX 10.4, *)
func CFNetServiceMonitorInvalidate(_ monitor: CFNetServiceMonitor)
@available(OSX 10.4, *)
@discardableResult
func CFNetServiceMonitorStart(_ monitor: CFNetServiceMonitor, _ recordType: CFNetServiceMonitorType, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(OSX 10.4, *)
func CFNetServiceMonitorStop(_ monitor: CFNetServiceMonitor, _ error: UnsafeMutablePointer<CFStreamError>?)
@available(OSX 10.4, *)
func CFNetServiceMonitorScheduleWithRunLoop(_ monitor: CFNetServiceMonitor, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
@available(OSX 10.4, *)
func CFNetServiceMonitorUnscheduleFromRunLoop(_ monitor: CFNetServiceMonitor, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
@available(OSX 10.2, *)
@discardableResult
func CFNetServiceBrowserCreate(_ alloc: CFAllocator?, _ clientCB: CFNetServiceBrowserClientCallBack, _ clientContext: UnsafeMutablePointer<CFNetServiceClientContext>) -> Unmanaged<CFNetServiceBrowser>
@available(OSX 10.2, *)
func CFNetServiceBrowserInvalidate(_ browser: CFNetServiceBrowser)
@available(OSX 10.2, *)
@discardableResult
func CFNetServiceBrowserSearchForDomains(_ browser: CFNetServiceBrowser, _ registrationDomains: Bool, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(OSX 10.2, *)
@discardableResult
func CFNetServiceBrowserSearchForServices(_ browser: CFNetServiceBrowser, _ domain: CFString, _ serviceType: CFString, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(OSX 10.2, *)
func CFNetServiceBrowserStopSearch(_ browser: CFNetServiceBrowser, _ error: UnsafeMutablePointer<CFStreamError>?)
@available(OSX 10.2, *)
func CFNetServiceBrowserScheduleWithRunLoop(_ browser: CFNetServiceBrowser, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
@available(OSX 10.2, *)
func CFNetServiceBrowserUnscheduleFromRunLoop(_ browser: CFNetServiceBrowser, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
