
typealias SecAccessOwnerType = UInt32
var kSecUseOnlyUID: Int { get }
var kSecUseOnlyGID: Int { get }
var kSecHonorRoot: Int { get }
var kSecMatchBits: Int { get }
@available(OSX 10.7, *)
let kSecACLAuthorizationAny: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationLogin: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationGenKey: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationDelete: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationExportWrapped: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationExportClear: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationImportWrapped: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationImportClear: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationSign: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationEncrypt: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationDecrypt: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationMAC: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationDerive: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationKeychainCreate: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationKeychainDelete: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationKeychainItemRead: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationKeychainItemInsert: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationKeychainItemModify: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationKeychainItemDelete: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationChangeACL: CFString
@available(OSX 10.7, *)
let kSecACLAuthorizationChangeOwner: CFString
@available(OSX 10.11, *)
let kSecACLAuthorizationPartitionID: CFString
@available(OSX 10.11, *)
let kSecACLAuthorizationIntegrity: CFString
@discardableResult
func SecAccessGetTypeID() -> CFTypeID
@discardableResult
func SecAccessCreate(_ descriptor: CFString, _ trustedlist: CFArray?, _ accessRef: UnsafeMutablePointer<SecAccess?>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func SecAccessCreateWithOwnerAndACL(_ userId: uid_t, _ groupId: gid_t, _ ownerType: SecAccessOwnerType, _ acls: CFArray?, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> SecAccess?
@available(OSX 10.7, *)
@discardableResult
func SecAccessCopyOwnerAndACL(_ accessRef: SecAccess, _ userId: UnsafeMutablePointer<uid_t>?, _ groupId: UnsafeMutablePointer<gid_t>?, _ ownerType: UnsafeMutablePointer<SecAccessOwnerType>?, _ aclList: UnsafeMutablePointer<CFArray?>?) -> OSStatus
@discardableResult
func SecAccessCopyACLList(_ accessRef: SecAccess, _ aclList: UnsafeMutablePointer<CFArray?>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func SecAccessCopyMatchingACLList(_ accessRef: SecAccess, _ authorizationTag: CFTypeRef) -> CFArray?
