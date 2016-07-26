
var kSecSubjectItemAttr: Int { get }
var kSecIssuerItemAttr: Int { get }
var kSecSerialNumberItemAttr: Int { get }
var kSecPublicKeyHashItemAttr: Int { get }
var kSecSubjectKeyIdentifierItemAttr: Int { get }
var kSecCertTypeItemAttr: Int { get }
var kSecCertEncodingItemAttr: Int { get }
@available(OSX 10.3, *)
@discardableResult
func SecCertificateGetTypeID() -> CFTypeID
@available(OSX 10.6, *)
@discardableResult
func SecCertificateCreateWithData(_ allocator: CFAllocator?, _ data: CFData) -> SecCertificate?
@available(OSX 10.3, *)
@discardableResult
func SecCertificateAddToKeychain(_ certificate: SecCertificate, _ keychain: SecKeychain?) -> OSStatus
@available(OSX 10.6, *)
@discardableResult
func SecCertificateCopyData(_ certificate: SecCertificate) -> CFData
@available(OSX 10.3, *)
@discardableResult
func SecCertificateCopyPublicKey(_ certificate: SecCertificate, _ key: UnsafeMutablePointer<SecKey?>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func SecCertificateCopyCommonName(_ certificate: SecCertificate, _ commonName: UnsafeMutablePointer<CFString?>) -> OSStatus
@available(OSX 10.6, *)
@discardableResult
func SecCertificateCopySubjectSummary(_ certificate: SecCertificate) -> CFString
@available(OSX 10.5, *)
@discardableResult
func SecCertificateCopyEmailAddresses(_ certificate: SecCertificate, _ emailAddresses: UnsafeMutablePointer<CFArray?>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func SecCertificateCopyPreferred(_ name: CFString, _ keyUsage: CFArray?) -> SecCertificate?
@available(OSX 10.5, *)
@discardableResult
func SecCertificateSetPreference(_ certificate: SecCertificate, _ name: CFString, _ keyUsage: uint32, _ date: CFDate?) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func SecCertificateSetPreferred(_ certificate: SecCertificate?, _ name: CFString, _ keyUsage: CFArray?) -> OSStatus
@available(OSX 10.7, *)
let kSecPropertyKeyType: CFString
@available(OSX 10.7, *)
let kSecPropertyKeyLabel: CFString
@available(OSX 10.7, *)
let kSecPropertyKeyLocalizedLabel: CFString
@available(OSX 10.7, *)
let kSecPropertyKeyValue: CFString
@available(OSX 10.7, *)
let kSecPropertyTypeWarning: CFString
@available(OSX 10.7, *)
let kSecPropertyTypeSuccess: CFString
@available(OSX 10.7, *)
let kSecPropertyTypeSection: CFString
@available(OSX 10.7, *)
let kSecPropertyTypeData: CFString
@available(OSX 10.7, *)
let kSecPropertyTypeString: CFString
@available(OSX 10.7, *)
let kSecPropertyTypeURL: CFString
@available(OSX 10.7, *)
let kSecPropertyTypeDate: CFString
@available(OSX 10.7, *)
@discardableResult
func SecCertificateCopyValues(_ certificate: SecCertificate, _ keys: CFArray?, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> CFDictionary?
@available(OSX 10.7, *)
let kSecCertificateUsageSigning: CFString
@available(OSX 10.7, *)
let kSecCertificateUsageSigningAndEncrypting: CFString
@available(OSX 10.7, *)
let kSecCertificateUsageDeriveAndSign: CFString
@available(OSX 10.7, *)
@discardableResult
func SecCertificateCopyLongDescription(_ alloc: CFAllocator?, _ certificate: SecCertificate, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> CFString?
@available(OSX 10.7, *)
@discardableResult
func SecCertificateCopyShortDescription(_ alloc: CFAllocator?, _ certificate: SecCertificate, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> CFString?
@available(OSX 10.7, *)
@discardableResult
func SecCertificateCopySerialNumber(_ certificate: SecCertificate, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> CFData?
@available(OSX 10.7, *)
@discardableResult
func SecCertificateCopyNormalizedIssuerContent(_ certificate: SecCertificate, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> CFData?
@available(OSX 10.7, *)
@discardableResult
func SecCertificateCopyNormalizedSubjectContent(_ certificate: SecCertificate, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> CFData?
