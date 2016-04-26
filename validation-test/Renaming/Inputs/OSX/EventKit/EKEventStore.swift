
enum EKSpan : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case thisEvent
  case futureEvents
}
typealias EKEventSearchCallback = (EKEvent, UnsafeMutablePointer<ObjCBool>) -> Void
@available(OSX 10.8, *)
class EKEventStore : NSObject {
  @available(OSX 10.9, *)
  @discardableResult
  class func authorizationStatus(for entityType: EKEntityType) -> EKAuthorizationStatus
  @available(OSX 10.11, *)
  init(sources sources: [EKSource])
  @available(OSX 10.9, *)
  func requestAccess(to entityType: EKEntityType, completion completion: EKEventStoreRequestAccessCompletionHandler)
  var eventStoreIdentifier: String { get }
  @available(OSX 10.11, *)
  var delegateSources: [EKSource] { get }
  @available(OSX 10.8, *)
  var sources: [EKSource] { get }
  @available(OSX 10.8, *)
  @discardableResult
  func source(withIdentifier identifier: String) -> EKSource
  @available(OSX 10.8, *)
  @discardableResult
  func calendars(for entityType: EKEntityType) -> [EKCalendar]
  var defaultCalendarForNewEvents: EKCalendar { get }
  @available(OSX 10.8, *)
  @discardableResult
  func defaultCalendarForNewReminders() -> EKCalendar
  @available(OSX 10.8, *)
  @discardableResult
  func calendar(withIdentifier identifier: String) -> EKCalendar?
  @available(OSX 10.8, *)
  func saveCalendar(_ calendar: EKCalendar, commit commit: Bool) throws
  @available(OSX 10.8, *)
  func removeCalendar(_ calendar: EKCalendar, commit commit: Bool) throws
  @available(OSX 10.8, *)
  @discardableResult
  func calendarItem(withIdentifier identifier: String) -> EKCalendarItem
  @available(OSX 10.8, *)
  @discardableResult
  func calendarItems(withExternalIdentifier externalIdentifier: String) -> [EKCalendarItem]
  @available(OSX 10.8, *)
  func save(_ event: EKEvent, span span: EKSpan, commit commit: Bool) throws
  @available(OSX 10.8, *)
  func remove(_ event: EKEvent, span span: EKSpan, commit commit: Bool) throws
  @discardableResult
  func event(withIdentifier identifier: String) -> EKEvent?
  @discardableResult
  func events(matching predicate: NSPredicate) -> [EKEvent]
  func enumerateEvents(matching predicate: NSPredicate, using block: EKEventSearchCallback)
  @discardableResult
  func predicateForEvents(withStart startDate: NSDate, end endDate: NSDate, calendars calendars: [EKCalendar]?) -> NSPredicate
  @available(OSX 10.8, *)
  func save(_ reminder: EKReminder, commit commit: Bool) throws
  @available(OSX 10.8, *)
  func remove(_ reminder: EKReminder, commit commit: Bool) throws
  @available(OSX 10.8, *)
  @discardableResult
  func fetchReminders(matching predicate: NSPredicate, completion completion: ([EKReminder]?) -> Void) -> AnyObject
  @available(OSX 10.8, *)
  func cancelFetchRequest(_ fetchIdentifier: AnyObject)
  @available(OSX 10.8, *)
  @discardableResult
  func predicateForReminders(in calendars: [EKCalendar]?) -> NSPredicate
  @available(OSX 10.8, *)
  @discardableResult
  func predicateForIncompleteReminders(withDueDateStarting startDate: NSDate?, ending endDate: NSDate?, calendars calendars: [EKCalendar]?) -> NSPredicate
  @available(OSX 10.8, *)
  @discardableResult
  func predicateForCompletedReminders(withCompletionDateStarting startDate: NSDate?, ending endDate: NSDate?, calendars calendars: [EKCalendar]?) -> NSPredicate
  @available(OSX 10.8, *)
  func commit() throws
  @available(OSX 10.8, *)
  func reset()
  @available(OSX 10.8, *)
  func refreshSourcesIfNecessary()
}
typealias EKEventStoreRequestAccessCompletionHandler = (Bool, NSError?) -> Void
@available(OSX 10.8, *)
let EKEventStoreChangedNotification: String
