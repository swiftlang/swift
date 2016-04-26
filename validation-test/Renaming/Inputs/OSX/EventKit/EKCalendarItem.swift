
@available(OSX 10.8, *)
class EKCalendarItem : EKObject {
  var calendar: EKCalendar
  @available(OSX 10.8, *)
  var calendarItemIdentifier: String { get }
  @available(OSX 10.8, *)
  var calendarItemExternalIdentifier: String { get }
  var title: String
  var location: String?
  var notes: String?
  @available(OSX 10.8, *)
  @NSCopying var url: NSURL?
  var lastModifiedDate: NSDate? { get }
  @available(OSX 10.8, *)
  var creationDate: NSDate? { get }
  @available(OSX 10.8, *)
  @NSCopying var timeZone: NSTimeZone?
  @available(OSX 10.8, *)
  var hasAlarms: Bool { get }
  @available(OSX 10.8, *)
  var hasRecurrenceRules: Bool { get }
  @available(OSX 10.8, *)
  var hasAttendees: Bool { get }
  @available(OSX 10.8, *)
  var hasNotes: Bool { get }
  var attendees: [EKParticipant]? { get }
  var alarms: [EKAlarm]?
  func addAlarm(_ alarm: EKAlarm)
  func removeAlarm(_ alarm: EKAlarm)
  @available(OSX 10.8, *)
  var recurrenceRules: [EKRecurrenceRule]?
  func addRecurrenceRule(_ rule: EKRecurrenceRule)
  func removeRecurrenceRule(_ rule: EKRecurrenceRule)
}
