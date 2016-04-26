
@available(iOS 5.0, *)
class EKCalendarItem : EKObject {
  var calendar: EKCalendar
  @available(iOS 6.0, *)
  var calendarItemIdentifier: String { get }
  @available(iOS 6.0, *)
  var calendarItemExternalIdentifier: String { get }
  var title: String
  var location: String?
  var notes: String?
  @available(iOS 5.0, *)
  @NSCopying var url: NSURL?
  var lastModifiedDate: NSDate? { get }
  @available(iOS 5.0, *)
  var creationDate: NSDate? { get }
  @available(iOS 5.0, *)
  @NSCopying var timeZone: NSTimeZone?
  @available(iOS 5.0, *)
  var hasAlarms: Bool { get }
  @available(iOS 5.0, *)
  var hasRecurrenceRules: Bool { get }
  @available(iOS 5.0, *)
  var hasAttendees: Bool { get }
  @available(iOS 5.0, *)
  var hasNotes: Bool { get }
  var attendees: [EKParticipant]? { get }
  var alarms: [EKAlarm]?
  func addAlarm(_ alarm: EKAlarm)
  func removeAlarm(_ alarm: EKAlarm)
  @available(iOS 5.0, *)
  var recurrenceRules: [EKRecurrenceRule]?
  func addRecurrenceRule(_ rule: EKRecurrenceRule)
  func removeRecurrenceRule(_ rule: EKRecurrenceRule)
}
