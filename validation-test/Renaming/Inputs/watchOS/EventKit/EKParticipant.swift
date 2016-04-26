
@available(watchOS 2.0, *)
class EKParticipant : EKObject, NSCopying {
  var url: NSURL { get }
  var name: String? { get }
  var participantStatus: EKParticipantStatus { get }
  var participantRole: EKParticipantRole { get }
  var participantType: EKParticipantType { get }
  @available(watchOS 2.0, *)
  var isCurrentUser: Bool { get }
  @available(watchOS 2.0, *)
  var contactPredicate: NSPredicate { get }
}
