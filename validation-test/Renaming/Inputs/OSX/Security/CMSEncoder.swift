
class CMSEncoder {
}
@available(OSX 10.5, *)
@discardableResult
func CMSEncoderGetTypeID() -> CFTypeID
@available(OSX 10.5, *)
@discardableResult
func CMSEncoderCreate(_ cmsEncoderOut: UnsafeMutablePointer<CMSEncoder?>) -> OSStatus
let kCMSEncoderDigestAlgorithmSHA1: CFString
let kCMSEncoderDigestAlgorithmSHA256: CFString
@available(OSX 10.11, *)
@discardableResult
func CMSEncoderSetSignerAlgorithm(_ cmsEncoder: CMSEncoder, _ digestAlgorithm: CFString) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSEncoderAddSigners(_ cmsEncoder: CMSEncoder, _ signerOrArray: CFTypeRef) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSEncoderCopySigners(_ cmsEncoder: CMSEncoder, _ signersOut: UnsafeMutablePointer<CFArray?>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSEncoderAddRecipients(_ cmsEncoder: CMSEncoder, _ recipientOrArray: CFTypeRef) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSEncoderCopyRecipients(_ cmsEncoder: CMSEncoder, _ recipientsOut: UnsafeMutablePointer<CFArray?>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSEncoderSetHasDetachedContent(_ cmsEncoder: CMSEncoder, _ detachedContent: Bool) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSEncoderGetHasDetachedContent(_ cmsEncoder: CMSEncoder, _ detachedContentOut: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSEncoderSetEncapsulatedContentType(_ cmsEncoder: CMSEncoder, _ eContentType: UnsafePointer<CSSM_OID>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSEncoderSetEncapsulatedContentTypeOID(_ cmsEncoder: CMSEncoder, _ eContentTypeOID: CFTypeRef) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSEncoderCopyEncapsulatedContentType(_ cmsEncoder: CMSEncoder, _ eContentTypeOut: UnsafeMutablePointer<CFData?>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSEncoderAddSupportingCerts(_ cmsEncoder: CMSEncoder, _ certOrArray: CFTypeRef) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSEncoderCopySupportingCerts(_ cmsEncoder: CMSEncoder, _ certsOut: UnsafeMutablePointer<CFArray?>) -> OSStatus
enum CMSSignedAttributes : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case attrNone
  case attrSmimeCapabilities
  case attrSmimeEncryptionKeyPrefs
  case attrSmimeMSEncryptionKeyPrefs
  case attrSigningTime
  case attrAppleCodesigningHashAgility
}
@available(OSX 10.5, *)
@discardableResult
func CMSEncoderAddSignedAttributes(_ cmsEncoder: CMSEncoder, _ signedAttributes: CMSSignedAttributes) -> OSStatus
enum CMSCertificateChainMode : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case none
  case signerOnly
  case chain
  case chainWithRoot
}
@available(OSX 10.5, *)
@discardableResult
func CMSEncoderSetCertificateChainMode(_ cmsEncoder: CMSEncoder, _ chainMode: CMSCertificateChainMode) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSEncoderGetCertificateChainMode(_ cmsEncoder: CMSEncoder, _ chainModeOut: UnsafeMutablePointer<CMSCertificateChainMode>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSEncoderUpdateContent(_ cmsEncoder: CMSEncoder, _ content: UnsafePointer<Void>, _ contentLen: Int) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSEncoderCopyEncodedContent(_ cmsEncoder: CMSEncoder, _ encodedContentOut: UnsafeMutablePointer<CFData?>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSEncode(_ signers: CFTypeRef?, _ recipients: CFTypeRef?, _ eContentType: UnsafePointer<CSSM_OID>?, _ detachedContent: Bool, _ signedAttributes: CMSSignedAttributes, _ content: UnsafePointer<Void>, _ contentLen: Int, _ encodedContentOut: UnsafeMutablePointer<CFData?>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMSEncodeContent(_ signers: CFTypeRef?, _ recipients: CFTypeRef?, _ eContentTypeOID: CFTypeRef?, _ detachedContent: Bool, _ signedAttributes: CMSSignedAttributes, _ content: UnsafePointer<Void>, _ contentLen: Int, _ encodedContentOut: UnsafeMutablePointer<CFData?>?) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func CMSEncoderCopySignerTimestamp(_ cmsEncoder: CMSEncoder, _ signerIndex: Int, _ timestamp: UnsafeMutablePointer<CFAbsoluteTime>) -> OSStatus
@available(OSX 10.10, *)
@discardableResult
func CMSEncoderCopySignerTimestampWithPolicy(_ cmsEncoder: CMSEncoder, _ timeStampPolicy: CFTypeRef?, _ signerIndex: Int, _ timestamp: UnsafeMutablePointer<CFAbsoluteTime>) -> OSStatus
