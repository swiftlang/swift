
@available(watchOS 20000, *)
class HMUser : NSObject {
  var name: String { get }
  @available(watchOS 2.0, *)
  @NSCopying var uniqueIdentifier: NSUUID { get }
}
