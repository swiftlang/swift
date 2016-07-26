
@available(OSX 10.8, *)
enum NSUserNotificationActivationType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case contentsClicked
  case actionButtonClicked
  @available(OSX 10.9, *)
  case replied
  @available(OSX 10.10, *)
  case additionalActionClicked
}
@available(OSX 10.8, *)
class NSUserNotification : NSObject, NSCopying {
  var title: String?
  var subtitle: String?
  var informativeText: String?
  var actionButtonTitle: String
  var userInfo: [String : AnyObject]?
  @NSCopying var deliveryDate: NSDate?
  @NSCopying var deliveryTimeZone: NSTimeZone?
  @NSCopying var deliveryRepeatInterval: NSDateComponents?
  @NSCopying var actualDeliveryDate: NSDate? { get }
  var isPresented: Bool { get }
  var isRemote: Bool { get }
  var soundName: String?
  var hasActionButton: Bool
  var activationType: NSUserNotificationActivationType { get }
  var otherButtonTitle: String
  @available(OSX 10.9, *)
  var identifier: String?
  @available(OSX 10.9, *)
  var hasReplyButton: Bool
  @available(OSX 10.9, *)
  var responsePlaceholder: String?
  @available(OSX 10.9, *)
  @NSCopying var response: NSAttributedString? { get }
  @available(OSX 10.10, *)
  var additionalActions: [NSUserNotificationAction]?
  @available(OSX 10.10, *)
  @NSCopying var additionalActivationAction: NSUserNotificationAction? { get }
}
@available(OSX 10.10, *)
class NSUserNotificationAction : NSObject, NSCopying {
  convenience init(identifier identifier: String?, title title: String?)
  var identifier: String? { get }
  var title: String? { get }
}
@available(OSX 10.8, *)
let NSUserNotificationDefaultSoundName: String
@available(OSX 10.8, *)
class NSUserNotificationCenter : NSObject {
  @discardableResult
  class func defaultUserNotificationCenter() -> NSUserNotificationCenter
  unowned(unsafe) var delegate: @sil_unmanaged NSUserNotificationCenterDelegate?
  var scheduledNotifications: [NSUserNotification]
  func scheduleNotification(_ notification: NSUserNotification)
  func removeScheduledNotification(_ notification: NSUserNotification)
  var deliveredNotifications: [NSUserNotification] { get }
  func deliver(_ notification: NSUserNotification)
  func removeDeliveredNotification(_ notification: NSUserNotification)
  func removeAllDeliveredNotifications()
}
protocol NSUserNotificationCenterDelegate : NSObjectProtocol {
  @available(OSX 10.8, *)
  optional func userNotificationCenter(_ center: NSUserNotificationCenter, didDeliver notification: NSUserNotification)
  @available(OSX 10.8, *)
  optional func userNotificationCenter(_ center: NSUserNotificationCenter, didActivate notification: NSUserNotification)
  @available(OSX 10.8, *)
  @discardableResult
  optional func userNotificationCenter(_ center: NSUserNotificationCenter, shouldPresent notification: NSUserNotification) -> Bool
}
