
class CFHTTPAuthentication {
}
enum CFStreamErrorHTTPAuthentication : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case typeUnsupported
  case badUserName
  case badPassword
}
@available(OSX 10.4, *)
let kCFHTTPAuthenticationUsername: CFString
@available(OSX 10.4, *)
let kCFHTTPAuthenticationPassword: CFString
@available(OSX 10.4, *)
let kCFHTTPAuthenticationAccountDomain: CFString
@available(OSX 10.2, *)
@discardableResult
func CFHTTPAuthenticationGetTypeID() -> CFTypeID
@available(OSX 10.2, *)
@discardableResult
func CFHTTPAuthenticationCreateFromResponse(_ alloc: CFAllocator?, _ response: CFHTTPMessage) -> Unmanaged<CFHTTPAuthentication>
@available(OSX 10.2, *)
@discardableResult
func CFHTTPAuthenticationIsValid(_ auth: CFHTTPAuthentication, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(OSX 10.2, *)
@discardableResult
func CFHTTPAuthenticationAppliesToRequest(_ auth: CFHTTPAuthentication, _ request: CFHTTPMessage) -> Bool
@available(OSX 10.2, *)
@discardableResult
func CFHTTPAuthenticationRequiresOrderedRequests(_ auth: CFHTTPAuthentication) -> Bool
@available(OSX 10.2, *)
@discardableResult
func CFHTTPMessageApplyCredentials(_ request: CFHTTPMessage, _ auth: CFHTTPAuthentication, _ username: CFString?, _ password: CFString?, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(OSX 10.4, *)
@discardableResult
func CFHTTPMessageApplyCredentialDictionary(_ request: CFHTTPMessage, _ auth: CFHTTPAuthentication, _ dict: CFDictionary, _ error: UnsafeMutablePointer<CFStreamError>?) -> Bool
@available(OSX 10.2, *)
@discardableResult
func CFHTTPAuthenticationCopyRealm(_ auth: CFHTTPAuthentication) -> Unmanaged<CFString>
@available(OSX 10.2, *)
@discardableResult
func CFHTTPAuthenticationCopyDomains(_ auth: CFHTTPAuthentication) -> Unmanaged<CFArray>
@available(OSX 10.2, *)
@discardableResult
func CFHTTPAuthenticationCopyMethod(_ auth: CFHTTPAuthentication) -> Unmanaged<CFString>
@available(OSX 10.3, *)
@discardableResult
func CFHTTPAuthenticationRequiresUserNameAndPassword(_ auth: CFHTTPAuthentication) -> Bool
@available(OSX 10.4, *)
@discardableResult
func CFHTTPAuthenticationRequiresAccountDomain(_ auth: CFHTTPAuthentication) -> Bool
