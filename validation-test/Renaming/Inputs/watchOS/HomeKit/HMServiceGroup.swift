
@available(watchOS 20000, *)
class HMServiceGroup : NSObject {
  var name: String { get }
  var services: [HMService] { get }
  @available(watchOS 2.0, *)
  @NSCopying var uniqueIdentifier: NSUUID { get }
}
