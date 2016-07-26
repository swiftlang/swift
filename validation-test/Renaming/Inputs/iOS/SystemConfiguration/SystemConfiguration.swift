
var kSCStatusOK: Int { get }
var kSCStatusFailed: Int { get }
var kSCStatusInvalidArgument: Int { get }
var kSCStatusAccessError: Int { get }
var kSCStatusNoKey: Int { get }
var kSCStatusKeyExists: Int { get }
var kSCStatusLocked: Int { get }
var kSCStatusNeedLock: Int { get }
var kSCStatusNoStoreSession: Int { get }
var kSCStatusNoStoreServer: Int { get }
var kSCStatusNotifierActive: Int { get }
var kSCStatusNoPrefsSession: Int { get }
var kSCStatusPrefsBusy: Int { get }
var kSCStatusNoConfigFile: Int { get }
var kSCStatusNoLink: Int { get }
var kSCStatusStale: Int { get }
var kSCStatusMaxLink: Int { get }
var kSCStatusReachabilityUnknown: Int { get }
var kSCStatusConnectionNoService: Int { get }
var kSCStatusConnectionIgnore: Int { get }
@available(iOS 2.0, *)
let kCFErrorDomainSystemConfiguration: CFString
@available(iOS 2.0, *)
@discardableResult
func SCCopyLastError() -> CFError
@available(iOS 2.0, *)
@discardableResult
func SCError() -> Int32
@available(iOS 2.0, *)
@discardableResult
func SCErrorString(_ status: Int32) -> UnsafePointer<Int8>
