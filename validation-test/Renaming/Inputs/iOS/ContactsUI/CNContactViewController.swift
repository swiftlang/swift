
@available(iOS 9.0, *)
class CNContactViewController : UIViewController {
  @discardableResult
  class func descriptorForRequiredKeys() -> CNKeyDescriptor
  convenience init(for contact: CNContact)
  convenience init(forUnknownContact contact: CNContact)
  convenience init(forNewContact contact: CNContact?)
  var contact: CNContact { get }
  var displayedPropertyKeys: [AnyObject]?
  weak var delegate: @sil_weak CNContactViewControllerDelegate?
  var contactStore: CNContactStore?
  var parentGroup: CNGroup?
  var parentContainer: CNContainer?
  var alternateName: String?
  var message: String?
  var allowsEditing: Bool
  var allowsActions: Bool
  var shouldShowLinkedContacts: Bool
  func highlightProperty(withKey key: String, identifier identifier: String?)
}
@available(iOS 9.0, *)
protocol CNContactViewControllerDelegate : NSObjectProtocol {
  @discardableResult
  optional func contactViewController(_ viewController: CNContactViewController, shouldPerformDefaultActionFor property: CNContactProperty) -> Bool
  optional func contactViewController(_ viewController: CNContactViewController, didCompleteWith contact: CNContact?)
}
