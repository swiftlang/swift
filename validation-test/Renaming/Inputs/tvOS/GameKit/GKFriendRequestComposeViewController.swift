
extension GKFriendRequestComposeViewController {
  @discardableResult
  class func maxNumberOfRecipients() -> Int
  func setMessage(_ message: String?)
  @available(tvOS 8.0, *)
  func addRecipientPlayers(_ players: [GKPlayer])
  func addRecipients(withEmailAddresses emailAddresses: [String])
  unowned(unsafe) var composeViewDelegate: @sil_unmanaged GKFriendRequestComposeViewControllerDelegate?
}
protocol GKFriendRequestComposeViewControllerDelegate {
}
