
typealias SecTrustResultType = UInt32
var kSecTrustResultInvalid: Int { get }
var kSecTrustResultProceed: Int { get }
@available(*, deprecated)
var kSecTrustResultConfirm: Int { get }
var kSecTrustResultDeny: Int { get }
var kSecTrustResultUnspecified: Int { get }
var kSecTrustResultRecoverableTrustFailure: Int { get }
var kSecTrustResultFatalTrustFailure: Int { get }
var kSecTrustResultOtherError: Int { get }
class SecTrust {
}
@available(watchOS 2.0, *)
let kSecPropertyTypeTitle: CFString
@available(watchOS 2.0, *)
let kSecPropertyTypeError: CFString
@available(watchOS 2.0, *)
let kSecTrustEvaluationDate: CFString
@available(watchOS 2.0, *)
let kSecTrustExtendedValidation: CFString
@available(watchOS 2.0, *)
let kSecTrustOrganizationName: CFString
@available(watchOS 2.0, *)
let kSecTrustResultValue: CFString
@available(watchOS 2.0, *)
let kSecTrustRevocationChecked: CFString
@available(watchOS 2.0, *)
let kSecTrustRevocationValidUntilDate: CFString
@available(watchOS 2.0, *)
let kSecTrustCertificateTransparency: CFString
typealias SecTrustCallback = (SecTrust, SecTrustResultType) -> Void
@available(watchOS 2.0, *)
@discardableResult
func SecTrustGetTypeID() -> CFTypeID
@available(watchOS 2.0, *)
@discardableResult
func SecTrustCreateWithCertificates(_ certificates: CFTypeRef, _ policies: CFTypeRef?, _ trust: UnsafeMutablePointer<SecTrust?>) -> OSStatus
@available(watchOS 2.0, *)
@discardableResult
func SecTrustSetPolicies(_ trust: SecTrust, _ policies: CFTypeRef) -> OSStatus
@available(watchOS 2.0, *)
@discardableResult
func SecTrustCopyPolicies(_ trust: SecTrust, _ policies: UnsafeMutablePointer<CFArray?>) -> OSStatus
@available(watchOS 2.0, *)
@discardableResult
func SecTrustSetNetworkFetchAllowed(_ trust: SecTrust, _ allowFetch: Bool) -> OSStatus
@available(watchOS 2.0, *)
@discardableResult
func SecTrustGetNetworkFetchAllowed(_ trust: SecTrust, _ allowFetch: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(watchOS 2.0, *)
@discardableResult
func SecTrustSetAnchorCertificates(_ trust: SecTrust, _ anchorCertificates: CFArray) -> OSStatus
@available(watchOS 2.0, *)
@discardableResult
func SecTrustSetAnchorCertificatesOnly(_ trust: SecTrust, _ anchorCertificatesOnly: Bool) -> OSStatus
@available(watchOS 2.0, *)
@discardableResult
func SecTrustCopyCustomAnchorCertificates(_ trust: SecTrust, _ anchors: UnsafeMutablePointer<CFArray?>) -> OSStatus
@available(watchOS 2.0, *)
@discardableResult
func SecTrustSetVerifyDate(_ trust: SecTrust, _ verifyDate: CFDate) -> OSStatus
@available(watchOS 2.0, *)
@discardableResult
func SecTrustGetVerifyTime(_ trust: SecTrust) -> CFAbsoluteTime
@available(watchOS 2.0, *)
@discardableResult
func SecTrustEvaluate(_ trust: SecTrust, _ result: UnsafeMutablePointer<SecTrustResultType>?) -> OSStatus
@available(watchOS 2.0, *)
@discardableResult
func SecTrustEvaluateAsync(_ trust: SecTrust, _ queue: dispatch_queue_t?, _ result: SecTrustCallback) -> OSStatus
@available(watchOS 2.0, *)
@discardableResult
func SecTrustGetTrustResult(_ trust: SecTrust, _ result: UnsafeMutablePointer<SecTrustResultType>) -> OSStatus
@available(watchOS 2.0, *)
@discardableResult
func SecTrustCopyPublicKey(_ trust: SecTrust) -> SecKey?
@available(watchOS 2.0, *)
@discardableResult
func SecTrustGetCertificateCount(_ trust: SecTrust) -> CFIndex
@available(watchOS 2.0, *)
@discardableResult
func SecTrustGetCertificateAtIndex(_ trust: SecTrust, _ ix: CFIndex) -> SecCertificate?
@available(watchOS 2.0, *)
@discardableResult
func SecTrustCopyExceptions(_ trust: SecTrust) -> CFData
@available(watchOS 2.0, *)
@discardableResult
func SecTrustSetExceptions(_ trust: SecTrust, _ exceptions: CFData) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func SecTrustCopyProperties(_ trust: SecTrust) -> CFArray?
@available(watchOS 2.0, *)
@discardableResult
func SecTrustCopyResult(_ trust: SecTrust) -> CFDictionary?
@available(watchOS 2.0, *)
@discardableResult
func SecTrustSetOCSPResponse(_ trust: SecTrust, _ responseData: CFTypeRef?) -> OSStatus
