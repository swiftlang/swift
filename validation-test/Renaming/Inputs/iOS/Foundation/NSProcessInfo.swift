
var NSWindowsNTOperatingSystem: Int { get }
var NSWindows95OperatingSystem: Int { get }
var NSSolarisOperatingSystem: Int { get }
var NSHPUXOperatingSystem: Int { get }
var NSMACHOperatingSystem: Int { get }
var NSSunOSOperatingSystem: Int { get }
var NSOSF1OperatingSystem: Int { get }
struct NSOperatingSystemVersion {
  var majorVersion: Int
  var minorVersion: Int
  var patchVersion: Int
  init()
  init(majorVersion majorVersion: Int, minorVersion minorVersion: Int, patchVersion patchVersion: Int)
}
class NSProcessInfo : NSObject {
  @discardableResult
  class func processInfo() -> NSProcessInfo
  var environment: [String : String] { get }
  var arguments: [String] { get }
  var hostName: String { get }
  var processName: String
  var processIdentifier: Int32 { get }
  var globallyUniqueString: String { get }
  @available(iOS, introduced: 2.0, deprecated: 8.0, message: "-operatingSystem always returns NSMACHOperatingSystem, use -operatingSystemVersion or -isOperatingSystemAtLeastVersion: instead")
  @discardableResult
  func operatingSystem() -> Int
  @available(iOS, introduced: 2.0, deprecated: 8.0, message: "-operatingSystemName always returns NSMACHOperatingSystem, use -operatingSystemVersionString instead")
  @discardableResult
  func operatingSystemName() -> String
  var operatingSystemVersionString: String { get }
  @available(iOS 8.0, *)
  var operatingSystemVersion: NSOperatingSystemVersion { get }
  @available(iOS 2.0, *)
  var processorCount: Int { get }
  @available(iOS 2.0, *)
  var activeProcessorCount: Int { get }
  @available(iOS 2.0, *)
  var physicalMemory: UInt64 { get }
  @available(iOS 8.0, *)
  @discardableResult
  func isOperatingSystemAtLeast(_ version: NSOperatingSystemVersion) -> Bool
  @available(iOS 4.0, *)
  var systemUptime: NSTimeInterval { get }
}
@available(iOS 7.0, *)
struct NSActivityOptions : OptionSet {
  init(rawValue rawValue: UInt64)
  let rawValue: UInt64
  static var idleDisplaySleepDisabled: NSActivityOptions { get }
  static var idleSystemSleepDisabled: NSActivityOptions { get }
  static var suddenTerminationDisabled: NSActivityOptions { get }
  static var automaticTerminationDisabled: NSActivityOptions { get }
  static var userInitiated: NSActivityOptions { get }
  static var userInitiatedAllowingIdleSystemSleep: NSActivityOptions { get }
  static var background: NSActivityOptions { get }
  static var latencyCritical: NSActivityOptions { get }
}
extension NSProcessInfo {
  @available(iOS 7.0, *)
  @discardableResult
  func beginActivity(_ options: NSActivityOptions = [], reason reason: String) -> NSObjectProtocol
  @available(iOS 7.0, *)
  func endActivity(_ activity: NSObjectProtocol)
  @available(iOS 7.0, *)
  func performActivity(_ options: NSActivityOptions = [], reason reason: String, using block: () -> Void)
  @available(iOS 8.2, *)
  func performExpiringActivity(withReason reason: String, using block: (Bool) -> Void)
}
extension NSProcessInfo {
}
extension NSProcessInfo {
  @available(iOS 9.0, *)
  var isLowPowerModeEnabled: Bool { get }
}
@available(iOS 9.0, *)
let NSProcessInfoPowerStateDidChangeNotification: String
