
@available(iOS 9.0, *)
class CNLabeledValue<ValueType : NSCopying, NSSecureCoding> : NSObject, NSCopying, NSSecureCoding {
  var identifier: String { get }
  var label: String { get }
  @NSCopying var value: ValueType { get }
  init(label label: String?, value value: ValueType)
  @discardableResult
  func settingLabel(_ label: String?) -> Self
  @discardableResult
  func settingValue(_ value: ValueType) -> Self
  @discardableResult
  func settingLabel(_ label: String?, value value: ValueType) -> Self
  @discardableResult
  class func localizedString(forLabel label: String) -> String
  init()
}
@available(iOS 9.0, *)
let CNLabelHome: String
@available(iOS 9.0, *)
let CNLabelWork: String
@available(iOS 9.0, *)
let CNLabelOther: String
@available(iOS 9.0, *)
let CNLabelEmailiCloud: String
@available(iOS 9.0, *)
let CNLabelURLAddressHomePage: String
@available(iOS 9.0, *)
let CNLabelDateAnniversary: String
