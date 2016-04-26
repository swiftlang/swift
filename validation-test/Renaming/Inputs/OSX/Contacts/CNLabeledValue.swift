
@available(OSX 10.11, *)
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
@available(OSX 10.11, *)
let CNLabelHome: String
@available(OSX 10.11, *)
let CNLabelWork: String
@available(OSX 10.11, *)
let CNLabelOther: String
@available(OSX 10.11, *)
let CNLabelEmailiCloud: String
@available(OSX 10.11, *)
let CNLabelURLAddressHomePage: String
@available(OSX 10.11, *)
let CNLabelDateAnniversary: String
