
@available(watchOS 2.0, *)
@discardableResult
func SecAccessControlGetTypeID() -> CFTypeID
@available(watchOS 2.0, *)
struct SecAccessControlCreateFlags : OptionSet {
  init(rawValue rawValue: CFIndex)
  let rawValue: CFIndex
  static var userPresence: SecAccessControlCreateFlags { get }
  @available(watchOS 2.0, *)
  static var touchIDAny: SecAccessControlCreateFlags { get }
  @available(watchOS 2.0, *)
  static var touchIDCurrentSet: SecAccessControlCreateFlags { get }
  @available(watchOS 2.0, *)
  static var devicePasscode: SecAccessControlCreateFlags { get }
  @available(watchOS 2.0, *)
  static var or: SecAccessControlCreateFlags { get }
  @available(watchOS 2.0, *)
  static var and: SecAccessControlCreateFlags { get }
  @available(watchOS 2.0, *)
  static var privateKeyUsage: SecAccessControlCreateFlags { get }
  @available(watchOS 2.0, *)
  static var applicationPassword: SecAccessControlCreateFlags { get }
}
@available(watchOS 2.0, *)
@discardableResult
func SecAccessControlCreateWithFlags(_ allocator: CFAllocator?, _ protection: CFTypeRef, _ flags: SecAccessControlCreateFlags, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> SecAccessControl?
