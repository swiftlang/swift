
@available(OSX 10.8, *)
class EKCalendar : EKObject {
  @available(OSX 10.8, *)
  /*not inherited*/ init(for entityType: EKEntityType, eventStore eventStore: EKEventStore)
  var source: EKSource
  @available(OSX 10.8, *)
  var calendarIdentifier: String { get }
  var title: String
  var type: EKCalendarType { get }
  var allowsContentModifications: Bool { get }
  @available(OSX 10.8, *)
  var isSubscribed: Bool { get }
  @available(OSX 10.8, *)
  var isImmutable: Bool { get }
  @NSCopying var color: NSColor
  var supportedEventAvailabilities: EKCalendarEventAvailabilityMask { get }
  @available(OSX 10.8, *)
  var allowedEntityTypes: EKEntityMask { get }
}
