
@available(tvOS 2.0, *)
@discardableResult
func SecIdentityGetTypeID() -> CFTypeID
@available(tvOS 2.0, *)
@discardableResult
func SecIdentityCopyCertificate(_ identityRef: SecIdentity, _ certificateRef: UnsafeMutablePointer<SecCertificate?>) -> OSStatus
@available(tvOS 2.0, *)
@discardableResult
func SecIdentityCopyPrivateKey(_ identityRef: SecIdentity, _ privateKeyRef: UnsafeMutablePointer<SecKey?>) -> OSStatus
