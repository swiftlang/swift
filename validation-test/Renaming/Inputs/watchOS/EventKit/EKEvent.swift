
enum EKEventAvailability : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notSupported
  case busy
  case free
  case tentative
  case unavailable
}
enum EKEventStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case confirmed
  case tentative
  case canceled
}
@available(watchOS 2.0, *)
class EKEvent : EKCalendarItem {
  /*not inherited*/ init(eventStore eventStore: EKEventStore)
  var eventIdentifier: String { get }
  var isAllDay: Bool
  @NSCopying var startDate: NSDate
  @NSCopying var endDate: NSDate
  @available(watchOS 2.0, *)
  @NSCopying var structuredLocation: EKStructuredLocation?
  @discardableResult
  func compareStartDate(with other: EKEvent) -> NSComparisonResult
  var organizer: EKParticipant? { get }
  var availability: EKEventAvailability
  var status: EKEventStatus { get }
  var isDetached: Bool { get }
  @available(watchOS 2.0, *)
  var occurrenceDate: NSDate { get }
  @available(watchOS 2.0, *)
  var birthdayContactIdentifier: String? { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use birthdayContactIdentifier instead")
  var birthdayPersonID: Int { get }
}
