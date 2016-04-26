
@available(iOS 2.0, *)
@discardableResult
func SecIdentityGetTypeID() -> CFTypeID
@available(iOS 2.0, *)
@discardableResult
func SecIdentityCopyCertificate(_ identityRef: SecIdentity, _ certificateRef: UnsafeMutablePointer<SecCertificate?>) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func SecIdentityCopyPrivateKey(_ identityRef: SecIdentity, _ privateKeyRef: UnsafeMutablePointer<SecKey?>) -> OSStatus
