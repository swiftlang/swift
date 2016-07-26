
class SCPreferences {
}
struct SCPreferencesNotification : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var commit: SCPreferencesNotification { get }
  static var apply: SCPreferencesNotification { get }
}
struct SCPreferencesContext {
  var version: CFIndex
  var info: UnsafeMutablePointer<Void>?
  var retain: (@convention(c) (UnsafePointer<Void>) -> UnsafePointer<Void>)?
  var release: (@convention(c) (UnsafePointer<Void>) -> Void)?
  var copyDescription: (@convention(c) (UnsafePointer<Void>) -> Unmanaged<CFString>)?
  init()
  init(version version: CFIndex, info info: UnsafeMutablePointer<Void>?, retain retain: (@convention(c) (UnsafePointer<Void>) -> UnsafePointer<Void>)?, release release: (@convention(c) (UnsafePointer<Void>) -> Void)?, copyDescription copyDescription: (@convention(c) (UnsafePointer<Void>) -> Unmanaged<CFString>)?)
}
typealias SCPreferencesCallBack = @convention(c) (SCPreferences, SCPreferencesNotification, UnsafeMutablePointer<Void>?) -> Void
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesGetTypeID() -> CFTypeID
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesCreate(_ allocator: CFAllocator?, _ name: CFString, _ prefsID: CFString?) -> SCPreferences?
@available(OSX 10.5, *)
@discardableResult
func SCPreferencesCreateWithAuthorization(_ allocator: CFAllocator?, _ name: CFString, _ prefsID: CFString?, _ authorization: AuthorizationRef?) -> SCPreferences?
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesLock(_ prefs: SCPreferences, _ wait: Bool) -> Bool
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesCommitChanges(_ prefs: SCPreferences) -> Bool
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesApplyChanges(_ prefs: SCPreferences) -> Bool
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesUnlock(_ prefs: SCPreferences) -> Bool
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesGetSignature(_ prefs: SCPreferences) -> CFData?
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesCopyKeyList(_ prefs: SCPreferences) -> CFArray?
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesGetValue(_ prefs: SCPreferences, _ key: CFString) -> CFPropertyList?
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesAddValue(_ prefs: SCPreferences, _ key: CFString, _ value: CFPropertyList) -> Bool
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesSetValue(_ prefs: SCPreferences, _ key: CFString, _ value: CFPropertyList) -> Bool
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesRemoveValue(_ prefs: SCPreferences, _ key: CFString) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCPreferencesSetCallback(_ prefs: SCPreferences, _ callout: SCPreferencesCallBack?, _ context: UnsafeMutablePointer<SCPreferencesContext>?) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCPreferencesScheduleWithRunLoop(_ prefs: SCPreferences, _ runLoop: CFRunLoop, _ runLoopMode: CFString) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCPreferencesUnscheduleFromRunLoop(_ prefs: SCPreferences, _ runLoop: CFRunLoop, _ runLoopMode: CFString) -> Bool
@available(OSX 10.6, *)
@discardableResult
func SCPreferencesSetDispatchQueue(_ prefs: SCPreferences, _ queue: dispatch_queue_t?) -> Bool
@available(OSX 10.4, *)
func SCPreferencesSynchronize(_ prefs: SCPreferences)
