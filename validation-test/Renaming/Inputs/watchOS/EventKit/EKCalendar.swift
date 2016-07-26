
@available(watchOS 2.0, *)
class EKCalendar : EKObject {
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  /*not inherited*/ init(eventStore eventStore: EKEventStore)
  @available(watchOS 2.0, *)
  /*not inherited*/ init(for entityType: EKEntityType, eventStore eventStore: EKEventStore)
  var source: EKSource
  @available(watchOS 2.0, *)
  var calendarIdentifier: String { get }
  var title: String
  var type: EKCalendarType { get }
  var allowsContentModifications: Bool { get }
  @available(watchOS 2.0, *)
  var isSubscribed: Bool { get }
  @available(watchOS 2.0, *)
  var isImmutable: Bool { get }
  var cgColor: CGColor
  var supportedEventAvailabilities: EKCalendarEventAvailabilityMask { get }
  @available(watchOS 2.0, *)
  var allowedEntityTypes: EKEntityMask { get }
}
