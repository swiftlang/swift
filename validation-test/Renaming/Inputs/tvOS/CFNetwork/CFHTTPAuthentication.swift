
class CFHTTPAuthentication {
}
enum CFStreamErrorHTTPAuthentication : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case typeUnsupported
  case badUserName
  case badPassword
}
@available(tvOS 2.0, *)
let kCFHTTPAuthenticationUsername: CFString
@available(tvOS 2.0, *)
let kCFHTTPAuthenticationPassword: CFString
@available(tvOS 2.0, *)
let kCFHTTPAuthenticationAccountDomain: CFString
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPAuthenticationGetTypeID() -> CFTypeID
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPAuthenticationCreateFromResponse(_ alloc: CFAllocator?, _ response: CFHTTPMessage) -> Unmanaged<CFHTTPAuthentication>
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPAuthenticationIsValid(_ auth: CFHTTPAuthentication, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPAuthenticationAppliesToRequest(_ auth: CFHTTPAuthentication, _ request: CFHTTPMessage) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPAuthenticationRequiresOrderedRequests(_ auth: CFHTTPAuthentication) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageApplyCredentials(_ request: CFHTTPMessage, _ auth: CFHTTPAuthentication, _ username: CFString?, _ password: CFString?, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageApplyCredentialDictionary(_ request: CFHTTPMessage, _ auth: CFHTTPAuthentication, _ dict: CFDictionary, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPAuthenticationCopyRealm(_ auth: CFHTTPAuthentication) -> Unmanaged<CFString>
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPAuthenticationCopyDomains(_ auth: CFHTTPAuthentication) -> Unmanaged<CFArray>
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPAuthenticationCopyMethod(_ auth: CFHTTPAuthentication) -> Unmanaged<CFString>
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPAuthenticationRequiresUserNameAndPassword(_ auth: CFHTTPAuthentication) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPAuthenticationRequiresAccountDomain(_ auth: CFHTTPAuthentication) -> Bool
