
@available(iOS 8.0, *)
class HMZone : NSObject {
  var name: String { get }
  var rooms: [HMRoom] { get }
  @available(iOS 9.0, *)
  @NSCopying var uniqueIdentifier: NSUUID { get }
  func updateName(_ name: String, completionHandler completion: (NSError?) -> Void)
  func addRoom(_ room: HMRoom, completionHandler completion: (NSError?) -> Void)
  func removeRoom(_ room: HMRoom, completionHandler completion: (NSError?) -> Void)
}
