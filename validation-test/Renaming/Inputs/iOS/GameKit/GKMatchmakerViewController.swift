
@available(iOS 4.1, *)
class GKMatchmakerViewController : UINavigationController {
  unowned(unsafe) var matchmakerDelegate: @sil_unmanaged GKMatchmakerViewControllerDelegate?
  var matchRequest: GKMatchRequest { get }
  var isHosted: Bool
  init?(matchRequest request: GKMatchRequest)
  init?(invite invite: GKInvite)
  @available(iOS 5.0, *)
  func addPlayers(to match: GKMatch)
  @available(iOS, introduced: 5.0, deprecated: 8.0, message: "use setHostedPlayer:didConnect:")
  func setHostedPlayer(_ playerID: String, connected connected: Bool)
  @available(iOS 8.0, *)
  func setHostedPlayer(_ player: GKPlayer, didConnect connected: Bool)
}
protocol GKMatchmakerViewControllerDelegate : NSObjectProtocol {
  @available(iOS 4.1, *)
  func matchmakerViewControllerWasCancelled(_ viewController: GKMatchmakerViewController)
  @available(iOS 4.1, *)
  func matchmakerViewController(_ viewController: GKMatchmakerViewController, didFailWithError error: NSError)
  @available(iOS 4.1, *)
  optional func matchmakerViewController(_ viewController: GKMatchmakerViewController, didFind match: GKMatch)
  @available(iOS 8.0, *)
  optional func matchmakerViewController(_ viewController: GKMatchmakerViewController, didFindHostedPlayers players: [GKPlayer])
  @available(iOS, introduced: 4.1, deprecated: 8.0, message: "use matchmakerViewController:didFindHostedPlayers:")
  optional func matchmakerViewController(_ viewController: GKMatchmakerViewController, didFindPlayers playerIDs: [String])
  @available(iOS 8.0, *)
  optional func matchmakerViewController(_ viewController: GKMatchmakerViewController, hostedPlayerDidAccept player: GKPlayer)
  @available(iOS, introduced: 5.0, deprecated: 8.0, message: "use matchmakerViewController:hostedPlayerDidAccept:")
  optional func matchmakerViewController(_ viewController: GKMatchmakerViewController, didReceiveAcceptFromHostedPlayer playerID: String)
}
