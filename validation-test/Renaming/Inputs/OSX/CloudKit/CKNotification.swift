
@available(OSX 10.10, *)
class CKNotificationID : NSObject, NSCopying, NSSecureCoding {
}
@available(OSX 10.10, *)
enum CKNotificationType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case query
  case recordZone
  case readNotification
}
@available(OSX 10.10, *)
class CKNotification : NSObject {
  convenience init(fromRemoteNotificationDictionary notificationDictionary: [String : NSObject])
  var notificationType: CKNotificationType { get }
  @NSCopying var notificationID: CKNotificationID? { get }
  var containerIdentifier: String? { get }
  var isPruned: Bool { get }
  var alertBody: String? { get }
  var alertLocalizationKey: String? { get }
  var alertLocalizationArgs: [String]? { get }
  var alertActionLocalizationKey: String? { get }
  var alertLaunchImage: String? { get }
  @NSCopying var badge: NSNumber? { get }
  var soundName: String? { get }
  @available(OSX 10.11, *)
  var subscriptionID: String? { get }
  @available(OSX 10.11, *)
  var category: String? { get }
}
@available(OSX 10.10, *)
enum CKQueryNotificationReason : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case recordCreated
  case recordUpdated
  case recordDeleted
}
@available(OSX 10.10, *)
class CKQueryNotification : CKNotification {
  var queryNotificationReason: CKQueryNotificationReason { get }
  var recordFields: [String : CKRecordValue]? { get }
  @NSCopying var recordID: CKRecordID? { get }
  var isPublicDatabase: Bool { get }
}
@available(OSX 10.10, *)
class CKRecordZoneNotification : CKNotification {
  @NSCopying var recordZoneID: CKRecordZoneID? { get }
}
