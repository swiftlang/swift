
typealias SecTrustResultType = UInt32
var kSecTrustResultInvalid: Int { get }
var kSecTrustResultProceed: Int { get }
var kSecTrustResultDeny: Int { get }
var kSecTrustResultUnspecified: Int { get }
var kSecTrustResultRecoverableTrustFailure: Int { get }
var kSecTrustResultFatalTrustFailure: Int { get }
var kSecTrustResultOtherError: Int { get }
class SecTrust {
}
@available(OSX 10.7, *)
let kSecPropertyTypeTitle: CFString
@available(OSX 10.7, *)
let kSecPropertyTypeError: CFString
@available(OSX 10.9, *)
let kSecTrustEvaluationDate: CFString
@available(OSX 10.9, *)
let kSecTrustExtendedValidation: CFString
@available(OSX 10.9, *)
let kSecTrustOrganizationName: CFString
@available(OSX 10.9, *)
let kSecTrustResultValue: CFString
@available(OSX 10.9, *)
let kSecTrustRevocationChecked: CFString
@available(OSX 10.9, *)
let kSecTrustRevocationValidUntilDate: CFString
typealias SecTrustCallback = (SecTrust, SecTrustResultType) -> Void
@available(OSX 10.3, *)
@discardableResult
func SecTrustGetTypeID() -> CFTypeID
@available(OSX 10.3, *)
@discardableResult
func SecTrustCreateWithCertificates(_ certificates: CFTypeRef, _ policies: CFTypeRef?, _ trust: UnsafeMutablePointer<SecTrust?>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func SecTrustSetPolicies(_ trust: SecTrust, _ policies: CFTypeRef) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func SecTrustCopyPolicies(_ trust: SecTrust, _ policies: UnsafeMutablePointer<CFArray?>) -> OSStatus
@available(OSX 10.9, *)
@discardableResult
func SecTrustSetNetworkFetchAllowed(_ trust: SecTrust, _ allowFetch: Bool) -> OSStatus
@available(OSX 10.9, *)
@discardableResult
func SecTrustGetNetworkFetchAllowed(_ trust: SecTrust, _ allowFetch: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func SecTrustSetAnchorCertificates(_ trust: SecTrust, _ anchorCertificates: CFArray) -> OSStatus
@available(OSX 10.6, *)
@discardableResult
func SecTrustSetAnchorCertificatesOnly(_ trust: SecTrust, _ anchorCertificatesOnly: Bool) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func SecTrustCopyCustomAnchorCertificates(_ trust: SecTrust, _ anchors: UnsafeMutablePointer<CFArray?>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func SecTrustSetVerifyDate(_ trust: SecTrust, _ verifyDate: CFDate) -> OSStatus
@available(OSX 10.6, *)
@discardableResult
func SecTrustGetVerifyTime(_ trust: SecTrust) -> CFAbsoluteTime
@available(OSX 10.3, *)
@discardableResult
func SecTrustEvaluate(_ trust: SecTrust, _ result: UnsafeMutablePointer<SecTrustResultType>?) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func SecTrustEvaluateAsync(_ trust: SecTrust, _ queue: dispatch_queue_t?, _ result: SecTrustCallback) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func SecTrustGetTrustResult(_ trust: SecTrust, _ result: UnsafeMutablePointer<SecTrustResultType>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func SecTrustCopyPublicKey(_ trust: SecTrust) -> SecKey?
@available(OSX 10.7, *)
@discardableResult
func SecTrustGetCertificateCount(_ trust: SecTrust) -> CFIndex
@available(OSX 10.7, *)
@discardableResult
func SecTrustGetCertificateAtIndex(_ trust: SecTrust, _ ix: CFIndex) -> SecCertificate?
@available(OSX 10.9, *)
@discardableResult
func SecTrustCopyExceptions(_ trust: SecTrust) -> CFData
@available(OSX 10.9, *)
@discardableResult
func SecTrustSetExceptions(_ trust: SecTrust, _ exceptions: CFData) -> Bool
@available(OSX 10.7, *)
@discardableResult
func SecTrustCopyProperties(_ trust: SecTrust) -> CFArray?
@available(OSX 10.9, *)
@discardableResult
func SecTrustCopyResult(_ trust: SecTrust) -> CFDictionary?
@available(OSX 10.9, *)
@discardableResult
func SecTrustSetOCSPResponse(_ trust: SecTrust, _ responseData: CFTypeRef?) -> OSStatus
struct SecTrustOptionFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var allowExpired: SecTrustOptionFlags { get }
  static var leafIsCA: SecTrustOptionFlags { get }
  static var fetchIssuerFromNet: SecTrustOptionFlags { get }
  static var allowExpiredRoot: SecTrustOptionFlags { get }
  static var requireRevPerCert: SecTrustOptionFlags { get }
  static var useTrustSettings: SecTrustOptionFlags { get }
  static var implicitAnchors: SecTrustOptionFlags { get }
}
@available(OSX 10.7, *)
@discardableResult
func SecTrustSetOptions(_ trustRef: SecTrust, _ options: SecTrustOptionFlags) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func SecTrustSetKeychains(_ trust: SecTrust, _ keychainOrArray: CFTypeRef?) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func SecTrustCopyAnchorCertificates(_ anchors: UnsafeMutablePointer<CFArray?>) -> OSStatus
