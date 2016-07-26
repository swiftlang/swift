
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use +[CNContactViewController viewControllerForUnknownContact:] from ContactsUI.framework instead")
class ABUnknownPersonViewController : UIViewController {
  unowned(unsafe) var unknownPersonViewDelegate: @sil_unmanaged ABUnknownPersonViewControllerDelegate?
  var addressBook: ABAddressBook?
  var displayedPerson: ABRecord
  var alternateName: String?
  var message: String?
  var allowsActions: Bool
  var allowsAddingToAddressBook: Bool
}
protocol ABUnknownPersonViewControllerDelegate : NSObjectProtocol {
  @available(iOS 2.0, *)
  func unknownPersonViewController(_ unknownCardViewController: ABUnknownPersonViewController, didResolveToPerson person: ABRecord?)
  @available(iOS 3.0, *)
  @discardableResult
  optional func unknownPersonViewController(_ personViewController: ABUnknownPersonViewController, shouldPerformDefaultActionForPerson person: ABRecord, property property: ABPropertyID, identifier identifier: ABMultiValueIdentifier) -> Bool
}
