
@available(iOS 9.0, *)
enum CNPostalAddressFormatterStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case mailingAddress
}
@available(iOS 9.0, *)
class CNPostalAddressFormatter : NSFormatter {
  @discardableResult
  class func string(from postalAddress: CNPostalAddress, style style: CNPostalAddressFormatterStyle) -> String
  @discardableResult
  class func attributedString(from postalAddress: CNPostalAddress, style style: CNPostalAddressFormatterStyle, withDefaultAttributes attributes: [NSObject : AnyObject] = [:]) -> NSAttributedString
  var style: CNPostalAddressFormatterStyle
  @discardableResult
  func string(from postalAddress: CNPostalAddress) -> String
  @discardableResult
  func attributedString(from postalAddress: CNPostalAddress, withDefaultAttributes attributes: [NSObject : AnyObject] = [:]) -> NSAttributedString
}
let CNPostalAddressPropertyAttribute: String
let CNPostalAddressLocalizedPropertyNameAttribute: String
