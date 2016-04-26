
var kSecTrustSettingsPolicy: String { get }
var kSecTrustSettingsApplication: String { get }
var kSecTrustSettingsPolicyString: String { get }
var kSecTrustSettingsKeyUsage: String { get }
var kSecTrustSettingsAllowedError: String { get }
var kSecTrustSettingsResult: String { get }
struct SecTrustSettingsKeyUsage : OptionSet {
  init(rawValue rawValue: uint32)
  let rawValue: uint32
  static var useSignature: SecTrustSettingsKeyUsage { get }
  static var useEnDecryptData: SecTrustSettingsKeyUsage { get }
  static var useEnDecryptKey: SecTrustSettingsKeyUsage { get }
  static var useSignCert: SecTrustSettingsKeyUsage { get }
  static var useSignRevocation: SecTrustSettingsKeyUsage { get }
  static var useKeyExchange: SecTrustSettingsKeyUsage { get }
  static var useAny: SecTrustSettingsKeyUsage { get }
}
enum SecTrustSettingsResult : uint32 {
  init?(rawValue rawValue: uint32)
  var rawValue: uint32 { get }
  case invalid
  case trustRoot
  case trustAsRoot
  case deny
  case unspecified
}
enum SecTrustSettingsDomain : uint32 {
  init?(rawValue rawValue: uint32)
  var rawValue: uint32 { get }
  case user
  case admin
  case system
}
@discardableResult
func SecTrustSettingsCopyTrustSettings(_ certRef: SecCertificate, _ domain: SecTrustSettingsDomain, _ trustSettings: UnsafeMutablePointer<CFArray?>) -> OSStatus
@discardableResult
func SecTrustSettingsSetTrustSettings(_ certRef: SecCertificate, _ domain: SecTrustSettingsDomain, _ trustSettingsDictOrArray: CFTypeRef?) -> OSStatus
@discardableResult
func SecTrustSettingsRemoveTrustSettings(_ certRef: SecCertificate, _ domain: SecTrustSettingsDomain) -> OSStatus
@discardableResult
func SecTrustSettingsCopyCertificates(_ domain: SecTrustSettingsDomain, _ certArray: UnsafeMutablePointer<CFArray?>?) -> OSStatus
@discardableResult
func SecTrustSettingsCopyModificationDate(_ certRef: SecCertificate, _ domain: SecTrustSettingsDomain, _ modificationDate: UnsafeMutablePointer<CFDate?>) -> OSStatus
@discardableResult
func SecTrustSettingsCreateExternalRepresentation(_ domain: SecTrustSettingsDomain, _ trustSettings: UnsafeMutablePointer<CFData?>) -> OSStatus
@discardableResult
func SecTrustSettingsImportExternalRepresentation(_ domain: SecTrustSettingsDomain, _ trustSettings: CFData) -> OSStatus
