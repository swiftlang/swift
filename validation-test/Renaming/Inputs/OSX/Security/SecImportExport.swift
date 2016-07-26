
enum SecExternalFormat : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case formatUnknown
  case formatOpenSSL
  case formatSSH
  case formatBSAFE
  case formatRawKey
  case formatWrappedPKCS8
  case formatWrappedOpenSSL
  case formatWrappedSSH
  case formatWrappedLSH
  case formatX509Cert
  case formatPEMSequence
  case formatPKCS7
  case formatPKCS12
  case formatNetscapeCertSequence
  case formatSSHv2
}
enum SecExternalItemType : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case itemTypeUnknown
  case itemTypePrivateKey
  case itemTypePublicKey
  case itemTypeSessionKey
  case itemTypeCertificate
  case itemTypeAggregate
}
struct SecItemImportExportFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var pemArmour: SecItemImportExportFlags { get }
}
struct SecKeyImportExportFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var importOnlyOne: SecKeyImportExportFlags { get }
  static var securePassphrase: SecKeyImportExportFlags { get }
  static var noAccessControl: SecKeyImportExportFlags { get }
}
var SEC_KEY_IMPORT_EXPORT_PARAMS_VERSION: Int32 { get }
struct SecKeyImportExportParameters {
  var version: UInt32
  var flags: SecKeyImportExportFlags
  var passphrase: Unmanaged<CFTypeRef>
  var alertTitle: Unmanaged<CFString>
  var alertPrompt: Unmanaged<CFString>
  var accessRef: Unmanaged<SecAccess>?
  var keyUsage: CSSM_KEYUSE
  var keyAttributes: CSSM_KEYATTR_FLAGS
}
struct SecItemImportExportKeyParameters {
  var version: UInt32
  var flags: SecKeyImportExportFlags
  var passphrase: Unmanaged<CFTypeRef>
  var alertTitle: Unmanaged<CFString>
  var alertPrompt: Unmanaged<CFString>
  var accessRef: Unmanaged<SecAccess>?
  var keyUsage: Unmanaged<CFArray>?
  var keyAttributes: Unmanaged<CFArray>?
}
@available(OSX 10.7, *)
@discardableResult
func SecItemExport(_ secItemOrArray: CFTypeRef, _ outputFormat: SecExternalFormat, _ flags: SecItemImportExportFlags, _ keyParams: UnsafePointer<SecItemImportExportKeyParameters>?, _ exportedData: UnsafeMutablePointer<CFData?>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func SecItemImport(_ importedData: CFData, _ fileNameOrExtension: CFString?, _ inputFormat: UnsafeMutablePointer<SecExternalFormat>?, _ itemType: UnsafeMutablePointer<SecExternalItemType>?, _ flags: SecItemImportExportFlags, _ keyParams: UnsafePointer<SecItemImportExportKeyParameters>?, _ importKeychain: SecKeychain?, _ outItems: UnsafeMutablePointer<CFArray?>?) -> OSStatus
let kSecImportExportPassphrase: CFString
let kSecImportExportKeychain: CFString
let kSecImportExportAccess: CFString
let kSecImportItemLabel: CFString
let kSecImportItemKeyID: CFString
let kSecImportItemTrust: CFString
let kSecImportItemCertChain: CFString
let kSecImportItemIdentity: CFString
@discardableResult
func SecPKCS12Import(_ pkcs12_data: CFData, _ options: CFDictionary, _ items: UnsafeMutablePointer<CFArray?>) -> OSStatus
