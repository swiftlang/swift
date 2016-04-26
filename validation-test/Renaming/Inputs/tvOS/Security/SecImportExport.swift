
@available(tvOS 2.0, *)
let kSecImportExportPassphrase: CFString
@available(tvOS 2.0, *)
let kSecImportItemLabel: CFString
@available(tvOS 2.0, *)
let kSecImportItemKeyID: CFString
@available(tvOS 2.0, *)
let kSecImportItemTrust: CFString
@available(tvOS 2.0, *)
let kSecImportItemCertChain: CFString
@available(tvOS 2.0, *)
let kSecImportItemIdentity: CFString
@available(tvOS 2.0, *)
@discardableResult
func SecPKCS12Import(_ pkcs12_data: CFData, _ options: CFDictionary, _ items: UnsafeMutablePointer<CFArray?>) -> OSStatus
