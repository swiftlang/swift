
@available(OSX 10.8, *)
class EKSource : EKObject {
  var sourceIdentifier: String { get }
  var sourceType: EKSourceType { get }
  var title: String { get }
  @available(OSX 10.8, *)
  @discardableResult
  func calendars(for entityType: EKEntityType) -> Set<EKCalendar>
}
