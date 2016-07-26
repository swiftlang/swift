
@available(OSX 10.8, *)
class GKMatchmakerViewController : NSViewController, GKViewController {
  unowned(unsafe) var matchmakerDelegate: @sil_unmanaged GKMatchmakerViewControllerDelegate?
  var matchRequest: GKMatchRequest { get }
  var isHosted: Bool
  init?(matchRequest request: GKMatchRequest)
  init?(invite invite: GKInvite)
  @available(OSX 10.8, *)
  func addPlayers(to match: GKMatch)
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "use setHostedPlayer:didConnect:")
  func setHostedPlayer(_ playerID: String, connected connected: Bool)
  @available(OSX 10.10, *)
  func setHostedPlayer(_ player: GKPlayer, didConnect connected: Bool)
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  var defaultInvitationMessage: String?
}
protocol GKMatchmakerViewControllerDelegate : NSObjectProtocol {
  @available(OSX 10.8, *)
  func matchmakerViewControllerWasCancelled(_ viewController: GKMatchmakerViewController)
  @available(OSX 10.8, *)
  func matchmakerViewController(_ viewController: GKMatchmakerViewController, didFailWithError error: NSError)
  @available(OSX 10.8, *)
  optional func matchmakerViewController(_ viewController: GKMatchmakerViewController, didFind match: GKMatch)
  @available(OSX 10.10, *)
  optional func matchmakerViewController(_ viewController: GKMatchmakerViewController, didFindHostedPlayers players: [GKPlayer])
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "use matchmakerViewController:didFindHostedPlayers:")
  optional func matchmakerViewController(_ viewController: GKMatchmakerViewController, didFindPlayers playerIDs: [String])
  @available(OSX 10.10, *)
  optional func matchmakerViewController(_ viewController: GKMatchmakerViewController, hostedPlayerDidAccept player: GKPlayer)
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "use matchmakerViewController:hostedPlayerDidAccept:")
  optional func matchmakerViewController(_ viewController: GKMatchmakerViewController, didReceiveAcceptFromHostedPlayer playerID: String)
}
