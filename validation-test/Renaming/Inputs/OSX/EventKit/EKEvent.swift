
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
@available(OSX 10.8, *)
class EKEvent : EKCalendarItem {
  /*not inherited*/ init(eventStore eventStore: EKEventStore)
  var eventIdentifier: String { get }
  var isAllDay: Bool
  @NSCopying var startDate: NSDate
  @NSCopying var endDate: NSDate
  @available(OSX 10.11, *)
  @NSCopying var structuredLocation: EKStructuredLocation?
  @discardableResult
  func compareStartDate(with other: EKEvent) -> NSComparisonResult
  var organizer: EKParticipant? { get }
  var availability: EKEventAvailability
  var status: EKEventStatus { get }
  var isDetached: Bool { get }
  @available(OSX 10.8, *)
  var occurrenceDate: NSDate { get }
  @available(OSX 10.11, *)
  var birthdayContactIdentifier: String? { get }
  @available(OSX, introduced: 10.8, deprecated: 10.11, message: "Use birthdayContactIdentifier instead")
  var birthdayPersonUniqueID: String? { get }
}
