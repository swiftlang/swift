
var errAuthorizationSuccess: OSStatus { get }
var errAuthorizationInvalidSet: OSStatus { get }
var errAuthorizationInvalidRef: OSStatus { get }
var errAuthorizationInvalidTag: OSStatus { get }
var errAuthorizationInvalidPointer: OSStatus { get }
var errAuthorizationDenied: OSStatus { get }
var errAuthorizationCanceled: OSStatus { get }
var errAuthorizationInteractionNotAllowed: OSStatus { get }
var errAuthorizationInternal: OSStatus { get }
var errAuthorizationExternalizeNotAllowed: OSStatus { get }
var errAuthorizationInternalizeNotAllowed: OSStatus { get }
var errAuthorizationInvalidFlags: OSStatus { get }
var errAuthorizationToolExecuteFailure: OSStatus { get }
var errAuthorizationToolEnvironmentError: OSStatus { get }
var errAuthorizationBadAddress: OSStatus { get }
struct AuthorizationFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var interactionAllowed: AuthorizationFlags { get }
  static var extendRights: AuthorizationFlags { get }
  static var partialRights: AuthorizationFlags { get }
  static var destroyRights: AuthorizationFlags { get }
  static var preAuthorize: AuthorizationFlags { get }
  static var noData: AuthorizationFlags { get }
}
var kAuthorizationFlagCanNotPreAuthorize: Int { get }
typealias AuthorizationRef = OpaquePointer
typealias AuthorizationString = UnsafePointer<Int8>
struct AuthorizationItem {
  var name: AuthorizationString
  var valueLength: Int
  var value: UnsafeMutablePointer<Void>
  var flags: UInt32
}
struct AuthorizationItemSet {
  var count: UInt32
  var items: UnsafeMutablePointer<AuthorizationItem>
}
var kAuthorizationExternalFormLength: Int { get }
struct AuthorizationExternalForm {
  var bytes: (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8)
  init()
  init(bytes bytes: (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8))
}
typealias AuthorizationRights = AuthorizationItemSet
typealias AuthorizationEnvironment = AuthorizationItemSet
@discardableResult
func AuthorizationCreate(_ rights: UnsafePointer<AuthorizationRights>?, _ environment: UnsafePointer<AuthorizationEnvironment>?, _ flags: AuthorizationFlags, _ authorization: UnsafeMutablePointer<AuthorizationRef?>?) -> OSStatus
@discardableResult
func AuthorizationFree(_ authorization: AuthorizationRef, _ flags: AuthorizationFlags) -> OSStatus
@discardableResult
func AuthorizationCopyRights(_ authorization: AuthorizationRef, _ rights: UnsafePointer<AuthorizationRights>, _ environment: UnsafePointer<AuthorizationEnvironment>?, _ flags: AuthorizationFlags, _ authorizedRights: UnsafeMutablePointer<UnsafeMutablePointer<AuthorizationRights>?>?) -> OSStatus
typealias AuthorizationAsyncCallback = (OSStatus, UnsafeMutablePointer<AuthorizationRights>?) -> Void
func AuthorizationCopyRightsAsync(_ authorization: AuthorizationRef, _ rights: UnsafePointer<AuthorizationRights>, _ environment: UnsafePointer<AuthorizationEnvironment>?, _ flags: AuthorizationFlags, _ callbackBlock: AuthorizationAsyncCallback)
@discardableResult
func AuthorizationCopyInfo(_ authorization: AuthorizationRef, _ tag: AuthorizationString?, _ info: UnsafeMutablePointer<UnsafeMutablePointer<AuthorizationItemSet>?>) -> OSStatus
@discardableResult
func AuthorizationMakeExternalForm(_ authorization: AuthorizationRef, _ extForm: UnsafeMutablePointer<AuthorizationExternalForm>) -> OSStatus
@discardableResult
func AuthorizationCreateFromExternalForm(_ extForm: UnsafePointer<AuthorizationExternalForm>, _ authorization: UnsafeMutablePointer<AuthorizationRef?>) -> OSStatus
@discardableResult
func AuthorizationFreeItemSet(_ set: UnsafeMutablePointer<AuthorizationItemSet>) -> OSStatus
