
enum GKInviteRecipientResponse : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case inviteRecipientResponseAccepted
  case inviteRecipientResponseDeclined
  case inviteRecipientResponseFailed
  case inviteRecipientResponseIncompatible
  case inviteRecipientResponseUnableToConnect
  case inviteRecipientResponseNoAnswer
  static var inviteeResponseAccepted: GKInviteRecipientResponse { get }
  static var inviteeResponseDeclined: GKInviteRecipientResponse { get }
  static var inviteeResponseFailed: GKInviteRecipientResponse { get }
  static var inviteeResponseIncompatible: GKInviteRecipientResponse { get }
  static var inviteeResponseUnableToConnect: GKInviteRecipientResponse { get }
  static var inviteeResponseNoAnswer: GKInviteRecipientResponse { get }
}
typealias GKInviteeResponse = GKInviteRecipientResponse
@available(tvOS 4.1, *)
class GKMatchRequest : NSObject {
  var minPlayers: Int
  var maxPlayers: Int
  var playerGroup: Int
  var playerAttributes: UInt32
  @available(tvOS 8.0, *)
  var recipients: [GKPlayer]?
  @available(tvOS 6.0, *)
  var inviteMessage: String?
  @available(tvOS 6.0, *)
  var defaultNumberOfPlayers: Int
  @available(tvOS 8.0, *)
  var recipientResponseHandler: ((GKPlayer, GKInviteRecipientResponse) -> Void)?
  @available(tvOS 6.0, *)
  @discardableResult
  class func maxPlayersAllowedForMatch(of matchType: GKMatchType) -> Int
}
enum GKMatchType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case peerToPeer
  case hosted
  case turnBased
}
@available(tvOS 4.1, *)
class GKInvite : NSObject {
  @available(tvOS 8.0, *)
  var sender: GKPlayer { get }
  var isHosted: Bool { get }
  @available(tvOS 6.0, *)
  var playerGroup: Int { get }
  @available(tvOS 6.0, *)
  var playerAttributes: UInt32 { get }
}
protocol GKInviteEventListener {
  @available(tvOS 7.0, *)
  optional func player(_ player: GKPlayer, didAccept invite: GKInvite)
  @available(tvOS 8.0, *)
  optional func player(_ player: GKPlayer, didRequestMatchWithRecipients recipientPlayers: [GKPlayer])
}
@available(tvOS 4.1, *)
class GKMatchmaker : NSObject {
  @discardableResult
  class func shared() -> GKMatchmaker
  @available(tvOS 6.0, *)
  func match(for invite: GKInvite, completionHandler completionHandler: ((GKMatch?, NSError?) -> Void)? = nil)
  func findMatch(for request: GKMatchRequest, withCompletionHandler completionHandler: ((GKMatch?, NSError?) -> Void)? = nil)
  @available(tvOS 8.0, *)
  func findPlayers(forHostedRequest request: GKMatchRequest, withCompletionHandler completionHandler: (([GKPlayer]?, NSError?) -> Void)? = nil)
  func addPlayers(to match: GKMatch, matchRequest matchRequest: GKMatchRequest, completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  func cancel()
  @available(tvOS 8.0, *)
  func cancelPendingInvite(to player: GKPlayer)
  @available(tvOS 6.0, *)
  func finishMatchmaking(for match: GKMatch)
  func queryPlayerGroupActivity(_ playerGroup: Int, withCompletionHandler completionHandler: ((Int, NSError?) -> Void)? = nil)
  func queryActivity(completionHandler completionHandler: ((Int, NSError?) -> Void)? = nil)
  @available(tvOS 8.0, *)
  func startBrowsingForNearbyPlayers(handler reachableHandler: ((GKPlayer, Bool) -> Void)? = nil)
  @available(tvOS 6.0, *)
  func stopBrowsingForNearbyPlayers()
}
extension GKMatchmaker {
}
