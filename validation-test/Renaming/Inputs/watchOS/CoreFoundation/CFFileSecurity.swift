
var __COREFOUNDATION_CFFILESECURITY__: Int32 { get }
class CFFileSecurity {
}
@available(watchOS 2.0, *)
@discardableResult
func CFFileSecurityGetTypeID() -> CFTypeID
@available(watchOS 2.0, *)
@discardableResult
func CFFileSecurityCreate(_ allocator: CFAllocator!) -> CFFileSecurity!
@available(watchOS 2.0, *)
@discardableResult
func CFFileSecurityCreateCopy(_ allocator: CFAllocator!, _ fileSec: CFFileSecurity!) -> CFFileSecurity!
@available(watchOS 2.0, *)
@discardableResult
func CFFileSecurityCopyOwnerUUID(_ fileSec: CFFileSecurity!, _ ownerUUID: UnsafeMutablePointer<Unmanaged<CFUUID>?>!) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CFFileSecuritySetOwnerUUID(_ fileSec: CFFileSecurity!, _ ownerUUID: CFUUID!) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CFFileSecurityCopyGroupUUID(_ fileSec: CFFileSecurity!, _ groupUUID: UnsafeMutablePointer<Unmanaged<CFUUID>?>!) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CFFileSecuritySetGroupUUID(_ fileSec: CFFileSecurity!, _ groupUUID: CFUUID!) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CFFileSecurityCopyAccessControlList(_ fileSec: CFFileSecurity!, _ accessControlList: UnsafeMutablePointer<acl_t?>!) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CFFileSecuritySetAccessControlList(_ fileSec: CFFileSecurity!, _ accessControlList: acl_t!) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CFFileSecurityGetOwner(_ fileSec: CFFileSecurity!, _ owner: UnsafeMutablePointer<uid_t>!) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CFFileSecuritySetOwner(_ fileSec: CFFileSecurity!, _ owner: uid_t) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CFFileSecurityGetGroup(_ fileSec: CFFileSecurity!, _ group: UnsafeMutablePointer<gid_t>!) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CFFileSecuritySetGroup(_ fileSec: CFFileSecurity!, _ group: gid_t) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CFFileSecurityGetMode(_ fileSec: CFFileSecurity!, _ mode: UnsafeMutablePointer<mode_t>!) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CFFileSecuritySetMode(_ fileSec: CFFileSecurity!, _ mode: mode_t) -> Bool
@available(watchOS 2.0, *)
struct CFFileSecurityClearOptions : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  static var owner: CFFileSecurityClearOptions { get }
  static var group: CFFileSecurityClearOptions { get }
  static var mode: CFFileSecurityClearOptions { get }
  static var ownerUUID: CFFileSecurityClearOptions { get }
  static var groupUUID: CFFileSecurityClearOptions { get }
  static var accessControlList: CFFileSecurityClearOptions { get }
}
@available(watchOS 2.0, *)
@discardableResult
func CFFileSecurityClearProperties(_ fileSec: CFFileSecurity!, _ clearPropertyMask: CFFileSecurityClearOptions) -> Bool
