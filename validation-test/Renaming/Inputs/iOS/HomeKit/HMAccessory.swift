
@available(iOS 8.0, *)
class HMAccessory : NSObject {
  var name: String { get }
  @available(iOS, introduced: 8.0, deprecated: 9.0)
  @NSCopying var identifier: NSUUID { get }
  @available(iOS 9.0, *)
  @NSCopying var uniqueIdentifier: NSUUID { get }
  weak var delegate: @sil_weak HMAccessoryDelegate?
  var isReachable: Bool { get }
  var isBridged: Bool { get }
  @available(iOS, introduced: 8.0, deprecated: 9.0)
  var identifiersForBridgedAccessories: [NSUUID]? { get }
  @available(iOS 9.0, *)
  var uniqueIdentifiersForBridgedAccessories: [NSUUID]? { get }
  @available(iOS 9.0, *)
  var category: HMAccessoryCategory { get }
  weak var room: @sil_weak HMRoom? { get }
  var services: [HMService] { get }
  var isBlocked: Bool { get }
  func updateName(_ name: String, completionHandler completion: (NSError?) -> Void)
  func identify(completionHandler completion: (NSError?) -> Void)
}
@available(iOS 8.0, *)
protocol HMAccessoryDelegate : NSObjectProtocol {
  optional func accessoryDidUpdateName(_ accessory: HMAccessory)
  optional func accessory(_ accessory: HMAccessory, didUpdateNameFor service: HMService)
  optional func accessory(_ accessory: HMAccessory, didUpdateAssociatedServiceTypeFor service: HMService)
  optional func accessoryDidUpdateServices(_ accessory: HMAccessory)
  optional func accessoryDidUpdateReachability(_ accessory: HMAccessory)
  optional func accessory(_ accessory: HMAccessory, service service: HMService, didUpdateValueFor characteristic: HMCharacteristic)
}
