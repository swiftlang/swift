
@available(tvOS 2.0, *)
let kCFHTTPVersion1_0: CFString
@available(tvOS 2.0, *)
let kCFHTTPVersion1_1: CFString
@available(tvOS 8.0, *)
let kCFHTTPVersion2_0: CFString
@available(tvOS 2.0, *)
let kCFHTTPAuthenticationSchemeBasic: CFString
@available(tvOS 2.0, *)
let kCFHTTPAuthenticationSchemeDigest: CFString
@available(tvOS 2.0, *)
let kCFHTTPAuthenticationSchemeNTLM: CFString
@available(tvOS 2.0, *)
let kCFHTTPAuthenticationSchemeKerberos: CFString
@available(tvOS 2.0, *)
let kCFHTTPAuthenticationSchemeNegotiate: CFString
@available(tvOS 3.0, *)
let kCFHTTPAuthenticationSchemeNegotiate2: CFString
@available(tvOS 4.3, *)
let kCFHTTPAuthenticationSchemeXMobileMeAuthToken: CFString
@available(tvOS 7.0, *)
let kCFHTTPAuthenticationSchemeOAuth1: CFString
class CFHTTPMessage {
}
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageGetTypeID() -> CFTypeID
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageCreateRequest(_ alloc: CFAllocator?, _ requestMethod: CFString, _ url: CFURL, _ httpVersion: CFString) -> Unmanaged<CFHTTPMessage>
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageCreateResponse(_ alloc: CFAllocator?, _ statusCode: CFIndex, _ statusDescription: CFString?, _ httpVersion: CFString) -> Unmanaged<CFHTTPMessage>
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageCreateEmpty(_ alloc: CFAllocator?, _ isRequest: Bool) -> Unmanaged<CFHTTPMessage>
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageCreateCopy(_ alloc: CFAllocator?, _ message: CFHTTPMessage) -> Unmanaged<CFHTTPMessage>
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageIsRequest(_ message: CFHTTPMessage) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageCopyVersion(_ message: CFHTTPMessage) -> Unmanaged<CFString>
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageCopyBody(_ message: CFHTTPMessage) -> Unmanaged<CFData>?
@available(tvOS 2.0, *)
func CFHTTPMessageSetBody(_ message: CFHTTPMessage, _ bodyData: CFData)
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageCopyHeaderFieldValue(_ message: CFHTTPMessage, _ headerField: CFString) -> Unmanaged<CFString>?
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageCopyAllHeaderFields(_ message: CFHTTPMessage) -> Unmanaged<CFDictionary>?
@available(tvOS 2.0, *)
func CFHTTPMessageSetHeaderFieldValue(_ message: CFHTTPMessage, _ headerField: CFString, _ value: CFString?)
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageAppendBytes(_ message: CFHTTPMessage, _ newBytes: UnsafePointer<UInt8>, _ numBytes: CFIndex) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageIsHeaderComplete(_ message: CFHTTPMessage) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageCopySerializedMessage(_ message: CFHTTPMessage) -> Unmanaged<CFData>?
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageCopyRequestURL(_ request: CFHTTPMessage) -> Unmanaged<CFURL>?
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageCopyRequestMethod(_ request: CFHTTPMessage) -> Unmanaged<CFString>?
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageAddAuthentication(_ request: CFHTTPMessage, _ authenticationFailureResponse: CFHTTPMessage?, _ username: CFString, _ password: CFString, _ authenticationScheme: CFString?, _ forProxy: Bool) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageGetResponseStatusCode(_ response: CFHTTPMessage) -> CFIndex
@available(tvOS 2.0, *)
@discardableResult
func CFHTTPMessageCopyResponseStatusLine(_ response: CFHTTPMessage) -> Unmanaged<CFString>?
