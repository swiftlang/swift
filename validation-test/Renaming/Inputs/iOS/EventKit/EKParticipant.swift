
@available(iOS 4.0, *)
class EKParticipant : EKObject, NSCopying {
  var url: NSURL { get }
  var name: String? { get }
  var participantStatus: EKParticipantStatus { get }
  var participantRole: EKParticipantRole { get }
  var participantType: EKParticipantType { get }
  @available(iOS 6.0, *)
  var isCurrentUser: Bool { get }
  @available(iOS 9.0, *)
  var contactPredicate: NSPredicate { get }
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use contactPredicate instead")
  @discardableResult
  func abRecord(with addressBook: ABAddressBook) -> ABRecord?
}
