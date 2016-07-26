
@available(iOS 4.2, *)
class GKFriendRequestComposeViewController : UINavigationController {
}
extension GKFriendRequestComposeViewController {
  @discardableResult
  class func maxNumberOfRecipients() -> Int
  func setMessage(_ message: String?)
  @available(iOS 8.0, *)
  func addRecipientPlayers(_ players: [GKPlayer])
  @available(iOS, introduced: 4.2, deprecated: 8.0, message: "use addRecipientPlayers:")
  func addRecipients(withPlayerIDs playerIDs: [String])
  func addRecipients(withEmailAddresses emailAddresses: [String])
  unowned(unsafe) var composeViewDelegate: @sil_unmanaged GKFriendRequestComposeViewControllerDelegate?
}
protocol GKFriendRequestComposeViewControllerDelegate {
  @available(iOS 4.2, *)
  func friendRequestComposeViewControllerDidFinish(_ viewController: GKFriendRequestComposeViewController)
}
