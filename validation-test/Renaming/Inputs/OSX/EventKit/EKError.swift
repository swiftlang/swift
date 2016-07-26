
@available(OSX 10.8, *)
let EKErrorDomain: String
enum EKErrorCode : Int {
  case eventNotMutable
  case noCalendar
  case noStartDate
  case noEndDate
  case datesInverted
  case internalFailure
  case calendarReadOnly
  case durationGreaterThanRecurrence
  case alarmGreaterThanRecurrence
  case startDateTooFarInFuture
  case startDateCollidesWithOtherOccurrence
  case objectBelongsToDifferentStore
  case invitesCannotBeMoved
  case invalidSpan
  case calendarHasNoSource
  case calendarSourceCannotBeModified
  case calendarIsImmutable
  case sourceDoesNotAllowCalendarAddDelete
  case recurringReminderRequiresDueDate
  case structuredLocationsNotSupported
  case reminderLocationsNotSupported
  case alarmProximityNotSupported
  case calendarDoesNotAllowEvents
  case calendarDoesNotAllowReminders
  case sourceDoesNotAllowReminders
  case sourceDoesNotAllowEvents
  case priorityIsInvalid
  case invalidEntityType
  case procedureAlarmsNotMutable
  case eventStoreNotAuthorized
  case osNotSupported
  case last
}

extension EKErrorCode : _BridgedNSError {
}
