
class SCNetworkConnection {
}
struct SCNetworkConnectionContext {
  var version: CFIndex
  var info: UnsafeMutablePointer<Void>?
  var retain: (@convention(c) (UnsafePointer<Void>) -> UnsafePointer<Void>)?
  var release: (@convention(c) (UnsafePointer<Void>) -> Void)?
  var copyDescription: (@convention(c) (UnsafePointer<Void>) -> Unmanaged<CFString>)?
  init()
  init(version version: CFIndex, info info: UnsafeMutablePointer<Void>?, retain retain: (@convention(c) (UnsafePointer<Void>) -> UnsafePointer<Void>)?, release release: (@convention(c) (UnsafePointer<Void>) -> Void)?, copyDescription copyDescription: (@convention(c) (UnsafePointer<Void>) -> Unmanaged<CFString>)?)
}
enum SCNetworkConnectionStatus : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case invalid
  case disconnected
  case connecting
  case connected
  case disconnecting
}
enum SCNetworkConnectionPPPStatus : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case disconnected
  case initializing
  case connectingLink
  case dialOnTraffic
  case negotiatingLink
  case authenticating
  case waitingForCallBack
  case negotiatingNetwork
  case connected
  case terminating
  case disconnectingLink
  case holdingLinkOff
  case suspended
  case waitingForRedial
}
typealias SCNetworkConnectionCallBack = @convention(c) (SCNetworkConnection, SCNetworkConnectionStatus, UnsafeMutablePointer<Void>?) -> Void
var kSCNetworkConnectionBytesIn: String { get }
var kSCNetworkConnectionBytesOut: String { get }
var kSCNetworkConnectionPacketsIn: String { get }
var kSCNetworkConnectionPacketsOut: String { get }
var kSCNetworkConnectionErrorsIn: String { get }
var kSCNetworkConnectionErrorsOut: String { get }
var kSCNetworkConnectionSelectionOptionOnDemandHostName: String { get }
var kSCNetworkConnectionSelectionOptionOnDemandRetry: String { get }
@available(OSX 10.3, *)
@discardableResult
func SCNetworkConnectionGetTypeID() -> CFTypeID
@available(OSX 10.3, *)
@discardableResult
func SCNetworkConnectionCopyUserPreferences(_ selectionOptions: CFDictionary?, _ serviceID: UnsafeMutablePointer<Unmanaged<CFString>>?, _ userOptions: UnsafeMutablePointer<Unmanaged<CFDictionary>>?) -> Bool
@available(OSX 10.3, *)
@discardableResult
func SCNetworkConnectionCreateWithServiceID(_ allocator: CFAllocator?, _ serviceID: CFString, _ callout: SCNetworkConnectionCallBack?, _ context: UnsafeMutablePointer<SCNetworkConnectionContext>?) -> SCNetworkConnection?
@available(OSX 10.3, *)
@discardableResult
func SCNetworkConnectionCopyServiceID(_ connection: SCNetworkConnection) -> CFString?
@available(OSX 10.3, *)
@discardableResult
func SCNetworkConnectionGetStatus(_ connection: SCNetworkConnection) -> SCNetworkConnectionStatus
@available(OSX 10.3, *)
@discardableResult
func SCNetworkConnectionCopyExtendedStatus(_ connection: SCNetworkConnection) -> CFDictionary?
@available(OSX 10.3, *)
@discardableResult
func SCNetworkConnectionCopyStatistics(_ connection: SCNetworkConnection) -> CFDictionary?
@available(OSX 10.3, *)
@discardableResult
func SCNetworkConnectionStart(_ connection: SCNetworkConnection, _ userOptions: CFDictionary?, _ linger: Bool) -> Bool
@available(OSX 10.3, *)
@discardableResult
func SCNetworkConnectionStop(_ connection: SCNetworkConnection, _ forceDisconnect: Bool) -> Bool
@available(OSX 10.3, *)
@discardableResult
func SCNetworkConnectionCopyUserOptions(_ connection: SCNetworkConnection) -> CFDictionary?
@available(OSX 10.3, *)
@discardableResult
func SCNetworkConnectionScheduleWithRunLoop(_ connection: SCNetworkConnection, _ runLoop: CFRunLoop, _ runLoopMode: CFString) -> Bool
@available(OSX 10.3, *)
@discardableResult
func SCNetworkConnectionUnscheduleFromRunLoop(_ connection: SCNetworkConnection, _ runLoop: CFRunLoop, _ runLoopMode: CFString) -> Bool
@available(OSX 10.6, *)
@discardableResult
func SCNetworkConnectionSetDispatchQueue(_ connection: SCNetworkConnection, _ queue: dispatch_queue_t?) -> Bool
