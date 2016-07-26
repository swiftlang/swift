
struct CBCharacteristicProperties : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var broadcast: CBCharacteristicProperties { get }
  static var read: CBCharacteristicProperties { get }
  static var writeWithoutResponse: CBCharacteristicProperties { get }
  static var write: CBCharacteristicProperties { get }
  static var notify: CBCharacteristicProperties { get }
  static var indicate: CBCharacteristicProperties { get }
  static var authenticatedSignedWrites: CBCharacteristicProperties { get }
  static var extendedProperties: CBCharacteristicProperties { get }
  @available(tvOS 6.0, *)
  static var notifyEncryptionRequired: CBCharacteristicProperties { get }
  @available(tvOS 6.0, *)
  static var indicateEncryptionRequired: CBCharacteristicProperties { get }
}
@available(tvOS 5.0, *)
class CBCharacteristic : CBAttribute {
  unowned(unsafe) var service: @sil_unmanaged CBService { get }
  var properties: CBCharacteristicProperties { get }
  var value: NSData? { get }
  var descriptors: [CBDescriptor]? { get }
  @available(tvOS, introduced: 5.0, deprecated: 8.0)
  var isBroadcasted: Bool { get }
  var isNotifying: Bool { get }
}
@available(tvOS 6.0, *)
struct CBAttributePermissions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var readable: CBAttributePermissions { get }
  static var writeable: CBAttributePermissions { get }
  static var readEncryptionRequired: CBAttributePermissions { get }
  static var writeEncryptionRequired: CBAttributePermissions { get }
}
@available(tvOS 6.0, *)
class CBMutableCharacteristic : CBCharacteristic {
  var permissions: CBAttributePermissions
  @available(tvOS 7.0, *)
  var subscribedCentrals: [CBCentral]? { get }
}
