
@available(iOS 9.0, *)
enum CNContactFormatterStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case fullName
  case phoneticFullName
}
@available(iOS 9.0, *)
enum CNContactDisplayNameOrder : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case userDefault
  case givenNameFirst
  case familyNameFirst
}
@available(iOS 9.0, *)
class CNContactFormatter : NSFormatter {
  @discardableResult
  class func descriptorForRequiredKeys(for style: CNContactFormatterStyle) -> CNKeyDescriptor
  @discardableResult
  class func string(from contact: CNContact, style style: CNContactFormatterStyle) -> String?
  @discardableResult
  class func attributedString(from contact: CNContact, style style: CNContactFormatterStyle, defaultAttributes attributes: [NSObject : AnyObject]? = [:]) -> NSAttributedString?
  @discardableResult
  class func nameOrder(for contact: CNContact) -> CNContactDisplayNameOrder
  @discardableResult
  class func delimiter(for contact: CNContact) -> String
  var style: CNContactFormatterStyle
  @discardableResult
  func string(from contact: CNContact) -> String?
  @discardableResult
  func attributedString(from contact: CNContact, defaultAttributes attributes: [NSObject : AnyObject]? = [:]) -> NSAttributedString?
}
let CNContactPropertyAttribute: String
