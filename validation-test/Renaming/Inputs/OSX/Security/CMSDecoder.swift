
class CMSDecoder {
}
@discardableResult
func CMSDecoderGetTypeID() -> CFTypeID
enum CMSSignerStatus : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case unsigned
  case valid
  case needsDetachedContent
  case invalidSignature
  case invalidCert
  case invalidIndex
}
@available(OSX 10.5, *)
@discardableResult
func CMSDecoderCreate(_ cmsDecoderOut: UnsafeMutablePointer<CMSDecoder?>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSDecoderUpdateMessage(_ cmsDecoder: CMSDecoder, _ msgBytes: UnsafePointer<Void>, _ msgBytesLen: Int) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSDecoderFinalizeMessage(_ cmsDecoder: CMSDecoder) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSDecoderSetDetachedContent(_ cmsDecoder: CMSDecoder, _ detachedContent: CFData) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSDecoderCopyDetachedContent(_ cmsDecoder: CMSDecoder, _ detachedContentOut: UnsafeMutablePointer<CFData?>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSDecoderSetSearchKeychain(_ cmsDecoder: CMSDecoder, _ keychainOrArray: CFTypeRef) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSDecoderGetNumSigners(_ cmsDecoder: CMSDecoder, _ numSignersOut: UnsafeMutablePointer<Int>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSDecoderCopySignerStatus(_ cmsDecoder: CMSDecoder, _ signerIndex: Int, _ policyOrArray: CFTypeRef, _ evaluateSecTrust: Bool, _ signerStatusOut: UnsafeMutablePointer<CMSSignerStatus>?, _ secTrustOut: UnsafeMutablePointer<SecTrust?>?, _ certVerifyResultCodeOut: UnsafeMutablePointer<OSStatus>?) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSDecoderCopySignerEmailAddress(_ cmsDecoder: CMSDecoder, _ signerIndex: Int, _ signerEmailAddressOut: UnsafeMutablePointer<CFString?>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSDecoderCopySignerCert(_ cmsDecoder: CMSDecoder, _ signerIndex: Int, _ signerCertOut: UnsafeMutablePointer<SecCertificate?>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSDecoderIsContentEncrypted(_ cmsDecoder: CMSDecoder, _ isEncryptedOut: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSDecoderCopyEncapsulatedContentType(_ cmsDecoder: CMSDecoder, _ eContentTypeOut: UnsafeMutablePointer<CFData?>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSDecoderCopyAllCerts(_ cmsDecoder: CMSDecoder, _ certsOut: UnsafeMutablePointer<CFArray?>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func CMSDecoderCopyContent(_ cmsDecoder: CMSDecoder, _ contentOut: UnsafeMutablePointer<CFData?>) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func CMSDecoderCopySignerSigningTime(_ cmsDecoder: CMSDecoder, _ signerIndex: Int, _ signingTime: UnsafeMutablePointer<CFAbsoluteTime>) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func CMSDecoderCopySignerTimestamp(_ cmsDecoder: CMSDecoder, _ signerIndex: Int, _ timestamp: UnsafeMutablePointer<CFAbsoluteTime>) -> OSStatus
@available(OSX 10.10, *)
@discardableResult
func CMSDecoderCopySignerTimestampWithPolicy(_ cmsDecoder: CMSDecoder, _ timeStampPolicy: CFTypeRef?, _ signerIndex: Int, _ timestamp: UnsafeMutablePointer<CFAbsoluteTime>) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func CMSDecoderCopySignerTimestampCertificates(_ cmsDecoder: CMSDecoder, _ signerIndex: Int, _ certificateRefs: UnsafeMutablePointer<CFArray?>) -> OSStatus
