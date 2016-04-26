
@available(iOS 4.0, *)
class EKCalendar : EKObject {
  @available(iOS 6.0, *)
  /*not inherited*/ init(for entityType: EKEntityType, eventStore eventStore: EKEventStore)
  var source: EKSource
  @available(iOS 5.0, *)
  var calendarIdentifier: String { get }
  var title: String
  var type: EKCalendarType { get }
  var allowsContentModifications: Bool { get }
  @available(iOS 5.0, *)
  var isSubscribed: Bool { get }
  @available(iOS 5.0, *)
  var isImmutable: Bool { get }
  var cgColor: CGColor
  var supportedEventAvailabilities: EKCalendarEventAvailabilityMask { get }
  @available(iOS 6.0, *)
  var allowedEntityTypes: EKEntityMask { get }
}
