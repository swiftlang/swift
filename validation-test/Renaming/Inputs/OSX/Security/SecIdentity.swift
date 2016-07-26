
@available(OSX 10.3, *)
@discardableResult
func SecIdentityGetTypeID() -> CFTypeID
@available(OSX 10.5, *)
@discardableResult
func SecIdentityCreateWithCertificate(_ keychainOrArray: CFTypeRef?, _ certificateRef: SecCertificate, _ identityRef: UnsafeMutablePointer<SecIdentity?>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func SecIdentityCopyCertificate(_ identityRef: SecIdentity, _ certificateRef: UnsafeMutablePointer<SecCertificate?>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func SecIdentityCopyPrivateKey(_ identityRef: SecIdentity, _ privateKeyRef: UnsafeMutablePointer<SecKey?>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func SecIdentityCopyPreferred(_ name: CFString, _ keyUsage: CFArray?, _ validIssuers: CFArray?) -> SecIdentity?
@available(OSX 10.7, *)
@discardableResult
func SecIdentitySetPreferred(_ identity: SecIdentity?, _ name: CFString, _ keyUsage: CFArray?) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func SecIdentityCopySystemIdentity(_ domain: CFString, _ idRef: UnsafeMutablePointer<SecIdentity?>, _ actualDomain: UnsafeMutablePointer<CFString?>?) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func SecIdentitySetSystemIdentity(_ domain: CFString, _ idRef: SecIdentity?) -> OSStatus
@available(OSX 10.5, *)
let kSecIdentityDomainDefault: CFString
@available(OSX 10.5, *)
let kSecIdentityDomainKerberosKDC: CFString
