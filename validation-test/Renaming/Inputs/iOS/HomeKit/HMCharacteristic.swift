
@available(iOS 8.0, *)
class HMCharacteristic : NSObject {
  var characteristicType: String { get }
  @available(iOS 9.0, *)
  var localizedDescription: String { get }
  weak var service: @sil_weak HMService? { get }
  var properties: [String] { get }
  var metadata: HMCharacteristicMetadata? { get }
  @NSCopying var value: AnyObject? { get }
  var isNotificationEnabled: Bool { get }
  @available(iOS 9.0, *)
  @NSCopying var uniqueIdentifier: NSUUID { get }
  func writeValue(_ value: AnyObject?, completionHandler completion: (NSError?) -> Void)
  func readValue(completionHandler completion: (NSError?) -> Void)
  func enableNotification(_ enable: Bool, completionHandler completion: (NSError?) -> Void)
  func updateAuthorizationData(_ data: NSData?, completionHandler completion: (NSError?) -> Void)
}
