
@available(iOS 2.0, *)
let kSecImportExportPassphrase: CFString
@available(iOS 2.0, *)
let kSecImportItemLabel: CFString
@available(iOS 2.0, *)
let kSecImportItemKeyID: CFString
@available(iOS 2.0, *)
let kSecImportItemTrust: CFString
@available(iOS 2.0, *)
let kSecImportItemCertChain: CFString
@available(iOS 2.0, *)
let kSecImportItemIdentity: CFString
@available(iOS 2.0, *)
@discardableResult
func SecPKCS12Import(_ pkcs12_data: CFData, _ options: CFDictionary, _ items: UnsafeMutablePointer<CFArray?>) -> OSStatus
