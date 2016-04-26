
@available(iOS 8.0, *)
class HMService : NSObject {
  weak var accessory: @sil_weak HMAccessory? { get }
  var serviceType: String { get }
  @available(iOS 9.0, *)
  var localizedDescription: String { get }
  var name: String { get }
  var associatedServiceType: String? { get }
  var characteristics: [HMCharacteristic] { get }
  @available(iOS 9.0, *)
  @NSCopying var uniqueIdentifier: NSUUID { get }
  @available(iOS 9.0, *)
  var isUserInteractive: Bool { get }
  func updateName(_ name: String, completionHandler completion: (NSError?) -> Void)
  func updateAssociatedServiceType(_ serviceType: String?, completionHandler completion: (NSError?) -> Void)
}
