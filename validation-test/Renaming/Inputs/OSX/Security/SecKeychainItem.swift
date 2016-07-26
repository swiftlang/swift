
enum SecItemClass : FourCharCode {
  init?(rawValue rawValue: FourCharCode)
  var rawValue: FourCharCode { get }
  case internetPasswordItemClass
  case genericPasswordItemClass
  case certificateItemClass
  case publicKeyItemClass
  case privateKeyItemClass
  case symmetricKeyItemClass
}
enum SecItemAttr : FourCharCode {
  init?(rawValue rawValue: FourCharCode)
  var rawValue: FourCharCode { get }
  case creationDateItemAttr
  case modDateItemAttr
  case descriptionItemAttr
  case commentItemAttr
  case creatorItemAttr
  case typeItemAttr
  case scriptCodeItemAttr
  case labelItemAttr
  case invisibleItemAttr
  case negativeItemAttr
  case customIconItemAttr
  case accountItemAttr
  case serviceItemAttr
  case genericItemAttr
  case securityDomainItemAttr
  case serverItemAttr
  case authenticationTypeItemAttr
  case portItemAttr
  case pathItemAttr
  case volumeItemAttr
  case addressItemAttr
  case signatureItemAttr
  case protocolItemAttr
  case certificateType
  case certificateEncoding
  case crlType
  case crlEncoding
  case alias
}
typealias SecAFPServerSignature = (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
typealias SecPublicKeyHash = (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
@discardableResult
func SecKeychainItemGetTypeID() -> CFTypeID
@discardableResult
func SecKeychainItemModifyAttributesAndData(_ itemRef: SecKeychainItem, _ attrList: UnsafePointer<SecKeychainAttributeList>?, _ length: UInt32, _ data: UnsafePointer<Void>?) -> OSStatus
@discardableResult
func SecKeychainItemCreateFromContent(_ itemClass: SecItemClass, _ attrList: UnsafeMutablePointer<SecKeychainAttributeList>, _ length: UInt32, _ data: UnsafePointer<Void>?, _ keychainRef: SecKeychain?, _ initialAccess: SecAccess?, _ itemRef: UnsafeMutablePointer<SecKeychainItem?>?) -> OSStatus
@discardableResult
func SecKeychainItemModifyContent(_ itemRef: SecKeychainItem, _ attrList: UnsafePointer<SecKeychainAttributeList>?, _ length: UInt32, _ data: UnsafePointer<Void>?) -> OSStatus
@discardableResult
func SecKeychainItemCopyContent(_ itemRef: SecKeychainItem, _ itemClass: UnsafeMutablePointer<SecItemClass>?, _ attrList: UnsafeMutablePointer<SecKeychainAttributeList>?, _ length: UnsafeMutablePointer<UInt32>?, _ outData: UnsafeMutablePointer<UnsafeMutablePointer<Void>?>?) -> OSStatus
@discardableResult
func SecKeychainItemFreeContent(_ attrList: UnsafeMutablePointer<SecKeychainAttributeList>?, _ data: UnsafeMutablePointer<Void>?) -> OSStatus
@discardableResult
func SecKeychainItemCopyAttributesAndData(_ itemRef: SecKeychainItem, _ info: UnsafeMutablePointer<SecKeychainAttributeInfo>?, _ itemClass: UnsafeMutablePointer<SecItemClass>?, _ attrList: UnsafeMutablePointer<UnsafeMutablePointer<SecKeychainAttributeList>?>?, _ length: UnsafeMutablePointer<UInt32>?, _ outData: UnsafeMutablePointer<UnsafeMutablePointer<Void>?>?) -> OSStatus
@discardableResult
func SecKeychainItemFreeAttributesAndData(_ attrList: UnsafeMutablePointer<SecKeychainAttributeList>?, _ data: UnsafeMutablePointer<Void>?) -> OSStatus
@discardableResult
func SecKeychainItemDelete(_ itemRef: SecKeychainItem) -> OSStatus
@discardableResult
func SecKeychainItemCopyKeychain(_ itemRef: SecKeychainItem, _ keychainRef: UnsafeMutablePointer<SecKeychain?>) -> OSStatus
@discardableResult
func SecKeychainItemCreateCopy(_ itemRef: SecKeychainItem, _ destKeychainRef: SecKeychain?, _ initialAccess: SecAccess, _ itemCopy: UnsafeMutablePointer<SecKeychainItem?>) -> OSStatus
@discardableResult
func SecKeychainItemCreatePersistentReference(_ itemRef: SecKeychainItem, _ persistentItemRef: UnsafeMutablePointer<CFData?>) -> OSStatus
@discardableResult
func SecKeychainItemCopyFromPersistentReference(_ persistentItemRef: CFData, _ itemRef: UnsafeMutablePointer<SecKeychainItem?>) -> OSStatus
@discardableResult
func SecKeychainItemCopyAccess(_ itemRef: SecKeychainItem, _ access: UnsafeMutablePointer<SecAccess?>) -> OSStatus
@discardableResult
func SecKeychainItemSetAccess(_ itemRef: SecKeychainItem, _ access: SecAccess) -> OSStatus
