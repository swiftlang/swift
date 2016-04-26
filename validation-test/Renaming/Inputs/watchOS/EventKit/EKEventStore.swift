
enum EKSpan : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case thisEvent
  case futureEvents
}
typealias EKEventSearchCallback = (EKEvent, UnsafeMutablePointer<ObjCBool>) -> Void
@available(watchOS 2.0, *)
class EKEventStore : NSObject {
  @available(watchOS 2.0, *)
  @discardableResult
  class func authorizationStatus(for entityType: EKEntityType) -> EKAuthorizationStatus
  @available(watchOS 2.0, *)
  func requestAccess(to entityType: EKEntityType, completion completion: EKEventStoreRequestAccessCompletionHandler)
  var eventStoreIdentifier: String { get }
  @available(watchOS 2.0, *)
  var sources: [EKSource] { get }
  @available(watchOS 2.0, *)
  @discardableResult
  func source(withIdentifier identifier: String) -> EKSource
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  var calendars: [EKCalendar] { get }
  @available(watchOS 2.0, *)
  @discardableResult
  func calendars(for entityType: EKEntityType) -> [EKCalendar]
  var defaultCalendarForNewEvents: EKCalendar { get }
  @available(watchOS 2.0, *)
  @discardableResult
  func defaultCalendarForNewReminders() -> EKCalendar
  @available(watchOS 2.0, *)
  @discardableResult
  func calendar(withIdentifier identifier: String) -> EKCalendar?
  @available(watchOS 2.0, *)
  @discardableResult
  func calendarItem(withIdentifier identifier: String) -> EKCalendarItem
  @available(watchOS 2.0, *)
  @discardableResult
  func calendarItems(withExternalIdentifier externalIdentifier: String) -> [EKCalendarItem]
  @discardableResult
  func event(withIdentifier identifier: String) -> EKEvent?
  @discardableResult
  func events(matching predicate: NSPredicate) -> [EKEvent]
  func enumerateEvents(matching predicate: NSPredicate, using block: EKEventSearchCallback)
  @discardableResult
  func predicateForEvents(withStart startDate: NSDate, end endDate: NSDate, calendars calendars: [EKCalendar]?) -> NSPredicate
  @available(watchOS 2.0, *)
  @discardableResult
  func fetchReminders(matching predicate: NSPredicate, completion completion: ([EKReminder]?) -> Void) -> AnyObject
  @available(watchOS 2.0, *)
  func cancelFetchRequest(_ fetchIdentifier: AnyObject)
  @available(watchOS 2.0, *)
  @discardableResult
  func predicateForReminders(in calendars: [EKCalendar]?) -> NSPredicate
  @available(watchOS 2.0, *)
  @discardableResult
  func predicateForIncompleteReminders(withDueDateStarting startDate: NSDate?, ending endDate: NSDate?, calendars calendars: [EKCalendar]?) -> NSPredicate
  @available(watchOS 2.0, *)
  @discardableResult
  func predicateForCompletedReminders(withCompletionDateStarting startDate: NSDate?, ending endDate: NSDate?, calendars calendars: [EKCalendar]?) -> NSPredicate
  @available(watchOS 2.0, *)
  func reset()
}
typealias EKEventStoreRequestAccessCompletionHandler = (Bool, NSError?) -> Void
@available(watchOS 2.0, *)
let EKEventStoreChangedNotification: String
