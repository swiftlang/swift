
@available(tvOS 8.0, *)
@discardableResult
func SecAccessControlGetTypeID() -> CFTypeID
@available(tvOS 8.0, *)
struct SecAccessControlCreateFlags : OptionSet {
  init(rawValue rawValue: CFIndex)
  let rawValue: CFIndex
  static var userPresence: SecAccessControlCreateFlags { get }
  @available(tvOS 9.0, *)
  static var touchIDAny: SecAccessControlCreateFlags { get }
  @available(tvOS 9.0, *)
  static var touchIDCurrentSet: SecAccessControlCreateFlags { get }
  @available(tvOS 9.0, *)
  static var devicePasscode: SecAccessControlCreateFlags { get }
  @available(tvOS 9.0, *)
  static var or: SecAccessControlCreateFlags { get }
  @available(tvOS 9.0, *)
  static var and: SecAccessControlCreateFlags { get }
  @available(tvOS 9.0, *)
  static var privateKeyUsage: SecAccessControlCreateFlags { get }
  @available(tvOS 9.0, *)
  static var applicationPassword: SecAccessControlCreateFlags { get }
}
@available(tvOS 8.0, *)
@discardableResult
func SecAccessControlCreateWithFlags(_ allocator: CFAllocator?, _ protection: CFTypeRef, _ flags: SecAccessControlCreateFlags, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> SecAccessControl?
