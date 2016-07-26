
@available(tvOS 8.0, *)
enum CKSubscriptionType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case query
  case recordZone
}
@available(tvOS 8.0, *)
struct CKSubscriptionOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var firesOnRecordCreation: CKSubscriptionOptions { get }
  static var firesOnRecordUpdate: CKSubscriptionOptions { get }
  static var firesOnRecordDeletion: CKSubscriptionOptions { get }
  static var firesOnce: CKSubscriptionOptions { get }
}
@available(tvOS 8.0, *)
class CKSubscription : NSObject, NSSecureCoding, NSCopying {
  convenience init(recordType recordType: String, predicate predicate: NSPredicate, options subscriptionOptions: CKSubscriptionOptions = [])
  init(recordType recordType: String, predicate predicate: NSPredicate, subscriptionID subscriptionID: String, options subscriptionOptions: CKSubscriptionOptions = [])
  convenience init(zoneID zoneID: CKRecordZoneID, options subscriptionOptions: CKSubscriptionOptions = [])
  init(zoneID zoneID: CKRecordZoneID, subscriptionID subscriptionID: String, options subscriptionOptions: CKSubscriptionOptions = [])
  var subscriptionID: String { get }
  var subscriptionType: CKSubscriptionType { get }
  var recordType: String? { get }
  @NSCopying var predicate: NSPredicate? { get }
  var subscriptionOptions: CKSubscriptionOptions { get }
  @NSCopying var notificationInfo: CKNotificationInfo?
  @NSCopying var zoneID: CKRecordZoneID?
}
@available(tvOS 8.0, *)
class CKNotificationInfo : NSObject, NSSecureCoding, NSCopying {
  var desiredKeys: [String]?
}
