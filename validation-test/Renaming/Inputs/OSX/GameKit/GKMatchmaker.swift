
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
@available(OSX 10.8, *)
class GKMatchRequest : NSObject {
  var minPlayers: Int
  var maxPlayers: Int
  var playerGroup: Int
  var playerAttributes: UInt32
  @available(OSX 10.10, *)
  var recipients: [GKPlayer]?
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "use recipients")
  var playersToInvite: [String]?
  @available(OSX 10.8, *)
  var inviteMessage: String?
  @available(OSX 10.8, *)
  var defaultNumberOfPlayers: Int
  @available(OSX 10.10, *)
  var recipientResponseHandler: ((GKPlayer, GKInviteRecipientResponse) -> Void)?
  @available(OSX, introduced: 10.9, deprecated: 10.10, message: "use recipientResponseHandler")
  var inviteeResponseHandler: ((String, GKInviteeResponse) -> Void)?
  @available(OSX 10.9, *)
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
@available(OSX 10.8, *)
class GKInvite : NSObject {
  @available(OSX 10.10, *)
  var sender: GKPlayer { get }
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "use sender")
  var inviter: String { get }
  var isHosted: Bool { get }
  @available(OSX 10.9, *)
  var playerGroup: Int { get }
  @available(OSX 10.9, *)
  var playerAttributes: UInt32 { get }
}
protocol GKInviteEventListener {
  @available(OSX 10.10, *)
  optional func player(_ player: GKPlayer, didAccept invite: GKInvite)
  @available(OSX 10.10, *)
  optional func player(_ player: GKPlayer, didRequestMatchWithRecipients recipientPlayers: [GKPlayer])
}
@available(OSX 10.8, *)
class GKMatchmaker : NSObject {
  @discardableResult
  class func shared() -> GKMatchmaker
  @available(OSX 10.9, *)
  func match(for invite: GKInvite, completionHandler completionHandler: ((GKMatch?, NSError?) -> Void)? = nil)
  func findMatch(for request: GKMatchRequest, withCompletionHandler completionHandler: ((GKMatch?, NSError?) -> Void)? = nil)
  @available(OSX 10.10, *)
  func findPlayers(forHostedRequest request: GKMatchRequest, withCompletionHandler completionHandler: (([GKPlayer]?, NSError?) -> Void)? = nil)
  func addPlayers(to match: GKMatch, matchRequest matchRequest: GKMatchRequest, completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  func cancel()
  @available(OSX 10.10, *)
  func cancelPendingInvite(to player: GKPlayer)
  @available(OSX 10.9, *)
  func finishMatchmaking(for match: GKMatch)
  func queryPlayerGroupActivity(_ playerGroup: Int, withCompletionHandler completionHandler: ((Int, NSError?) -> Void)? = nil)
  func queryActivity(completionHandler completionHandler: ((Int, NSError?) -> Void)? = nil)
  @available(OSX 10.10, *)
  func startBrowsingForNearbyPlayers(handler reachableHandler: ((GKPlayer, Bool) -> Void)? = nil)
  @available(OSX 10.9, *)
  func stopBrowsingForNearbyPlayers()
}
extension GKMatchmaker {
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "Use registerListener on GKLocalPlayer to register an object that implements the GKInviteEventListenerProtocol instead")
  var inviteHandler: ((GKInvite, [AnyObject]?) -> Void)?
  @available(OSX, introduced: 10.9, deprecated: 10.10)
  func startBrowsingForNearbyPlayers(reachableHandler reachableHandler: ((String, Bool) -> Void)? = nil)
  @available(OSX, introduced: 10.9, deprecated: 10.10, message: "use cancelPendingInviteToPlayer:")
  func cancelInvite(toPlayer playerID: String)
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "use findPlayersForHostedRequest:")
  func findPlayers(forHostedMatchRequest request: GKMatchRequest, withCompletionHandler completionHandler: (([String]?, NSError?) -> Void)? = nil)
}
