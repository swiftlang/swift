
@available(OSX 10.8, *)
class GKFriendRequestComposeViewController : NSViewController, GKViewController {
}
extension GKFriendRequestComposeViewController {
  @discardableResult
  class func maxNumberOfRecipients() -> Int
  func setMessage(_ message: String?)
  @available(OSX 10.10, *)
  func addRecipientPlayers(_ players: [GKPlayer])
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "use addRecipientPlayers:")
  func addRecipients(withPlayerIDs playerIDs: [String])
  func addRecipients(withEmailAddresses emailAddresses: [String])
  unowned(unsafe) var composeViewDelegate: @sil_unmanaged GKFriendRequestComposeViewControllerDelegate?
}
protocol GKFriendRequestComposeViewControllerDelegate {
  @available(OSX 10.8, *)
  func friendRequestComposeViewControllerDidFinish(_ viewController: GKFriendRequestComposeViewController)
}
