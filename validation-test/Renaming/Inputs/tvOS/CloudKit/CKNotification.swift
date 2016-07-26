
@available(tvOS 8.0, *)
class CKNotificationID : NSObject, NSCopying, NSSecureCoding {
}
@available(tvOS 8.0, *)
enum CKNotificationType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case query
  case recordZone
  case readNotification
}
@available(tvOS 8.0, *)
class CKNotification : NSObject {
  convenience init(fromRemoteNotificationDictionary notificationDictionary: [String : NSObject])
  var notificationType: CKNotificationType { get }
  @NSCopying var notificationID: CKNotificationID? { get }
  var containerIdentifier: String? { get }
  var isPruned: Bool { get }
  @available(tvOS 9.0, *)
  var subscriptionID: String? { get }
}
@available(tvOS 8.0, *)
enum CKQueryNotificationReason : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case recordCreated
  case recordUpdated
  case recordDeleted
}
@available(tvOS 8.0, *)
class CKQueryNotification : CKNotification {
  var queryNotificationReason: CKQueryNotificationReason { get }
  var recordFields: [String : CKRecordValue]? { get }
  @NSCopying var recordID: CKRecordID? { get }
  var isPublicDatabase: Bool { get }
}
@available(tvOS 8.0, *)
class CKRecordZoneNotification : CKNotification {
  @NSCopying var recordZoneID: CKRecordZoneID? { get }
}
