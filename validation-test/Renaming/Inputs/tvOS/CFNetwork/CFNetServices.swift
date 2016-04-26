
class CFNetService {
}
class CFNetServiceMonitor {
}
class CFNetServiceBrowser {
}
@available(tvOS 2.0, *)
let kCFStreamErrorDomainMach: Int32
@available(tvOS 2.0, *)
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
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceGetTypeID() -> CFTypeID
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceMonitorGetTypeID() -> CFTypeID
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceBrowserGetTypeID() -> CFTypeID
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceCreate(_ alloc: CFAllocator?, _ domain: CFString, _ serviceType: CFString, _ name: CFString, _ port: Int32) -> Unmanaged<CFNetService>
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceCreateCopy(_ alloc: CFAllocator?, _ service: CFNetService) -> Unmanaged<CFNetService>
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceGetDomain(_ theService: CFNetService) -> Unmanaged<CFString>
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceGetType(_ theService: CFNetService) -> Unmanaged<CFString>
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceGetName(_ theService: CFNetService) -> Unmanaged<CFString>
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceRegisterWithOptions(_ theService: CFNetService, _ options: CFOptionFlags, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceResolveWithTimeout(_ theService: CFNetService, _ timeout: CFTimeInterval, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(tvOS 2.0, *)
func CFNetServiceCancel(_ theService: CFNetService)
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceGetTargetHost(_ theService: CFNetService) -> Unmanaged<CFString>?
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceGetPortNumber(_ theService: CFNetService) -> Int32
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceGetAddressing(_ theService: CFNetService) -> Unmanaged<CFArray>?
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceGetTXTData(_ theService: CFNetService) -> Unmanaged<CFData>?
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceSetTXTData(_ theService: CFNetService, _ txtRecord: CFData) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceCreateDictionaryWithTXTData(_ alloc: CFAllocator?, _ txtRecord: CFData) -> Unmanaged<CFDictionary>?
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceCreateTXTDataWithDictionary(_ alloc: CFAllocator?, _ keyValuePairs: CFDictionary) -> Unmanaged<CFData>?
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceSetClient(_ theService: CFNetService, _ clientCB: CFNetServiceClientCallBack?, _ clientContext: UnsafeMutablePointer<CFNetServiceClientContext>?) -> Bool
@available(tvOS 2.0, *)
func CFNetServiceScheduleWithRunLoop(_ theService: CFNetService, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
@available(tvOS 2.0, *)
func CFNetServiceUnscheduleFromRunLoop(_ theService: CFNetService, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceMonitorCreate(_ alloc: CFAllocator?, _ theService: CFNetService, _ clientCB: CFNetServiceMonitorClientCallBack, _ clientContext: UnsafeMutablePointer<CFNetServiceClientContext>) -> Unmanaged<CFNetServiceMonitor>
@available(tvOS 2.0, *)
func CFNetServiceMonitorInvalidate(_ monitor: CFNetServiceMonitor)
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceMonitorStart(_ monitor: CFNetServiceMonitor, _ recordType: CFNetServiceMonitorType, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(tvOS 2.0, *)
func CFNetServiceMonitorStop(_ monitor: CFNetServiceMonitor, _ error: UnsafeMutablePointer<CFStreamError>?)
@available(tvOS 2.0, *)
func CFNetServiceMonitorScheduleWithRunLoop(_ monitor: CFNetServiceMonitor, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
@available(tvOS 2.0, *)
func CFNetServiceMonitorUnscheduleFromRunLoop(_ monitor: CFNetServiceMonitor, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceBrowserCreate(_ alloc: CFAllocator?, _ clientCB: CFNetServiceBrowserClientCallBack, _ clientContext: UnsafeMutablePointer<CFNetServiceClientContext>) -> Unmanaged<CFNetServiceBrowser>
@available(tvOS 2.0, *)
func CFNetServiceBrowserInvalidate(_ browser: CFNetServiceBrowser)
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceBrowserSearchForDomains(_ browser: CFNetServiceBrowser, _ registrationDomains: Bool, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CFNetServiceBrowserSearchForServices(_ browser: CFNetServiceBrowser, _ domain: CFString, _ serviceType: CFString, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(tvOS 2.0, *)
func CFNetServiceBrowserStopSearch(_ browser: CFNetServiceBrowser, _ error: UnsafeMutablePointer<CFStreamError>?)
@available(tvOS 2.0, *)
func CFNetServiceBrowserScheduleWithRunLoop(_ browser: CFNetServiceBrowser, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
@available(tvOS 2.0, *)
func CFNetServiceBrowserUnscheduleFromRunLoop(_ browser: CFNetServiceBrowser, _ runLoop: CFRunLoop, _ runLoopMode: CFString)
