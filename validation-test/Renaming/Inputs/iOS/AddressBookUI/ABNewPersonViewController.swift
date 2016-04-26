
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use +[CNContactViewController viewControllerForNewContact:] from ContactsUI.framework instead")
class ABNewPersonViewController : UIViewController {
  unowned(unsafe) var newPersonViewDelegate: @sil_unmanaged ABNewPersonViewControllerDelegate?
  var addressBook: ABAddressBook?
  var displayedPerson: ABRecord?
  var parentGroup: ABRecord?
}
protocol ABNewPersonViewControllerDelegate : NSObjectProtocol {
  @available(iOS 2.0, *)
  func newPersonViewController(_ newPersonView: ABNewPersonViewController, didCompleteWithNewPerson person: ABRecord?)
}
