
@available(iOS 8.0, *)
struct PKMerchantCapability : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var capability3DS: PKMerchantCapability { get }
  static var capabilityEMV: PKMerchantCapability { get }
  @available(iOS 9.0, *)
  static var capabilityCredit: PKMerchantCapability { get }
  @available(iOS 9.0, *)
  static var capabilityDebit: PKMerchantCapability { get }
}
@available(iOS 8.0, *)
struct PKAddressField : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var postalAddress: PKAddressField { get }
  static var phone: PKAddressField { get }
  static var email: PKAddressField { get }
  @available(iOS 8.3, *)
  static var name: PKAddressField { get }
  static var all: PKAddressField { get }
}
@available(iOS 8.3, *)
enum PKShippingType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case shipping
  case delivery
  case storePickup
  case servicePickup
}
@available(iOS 9.0, *)
enum PKPaymentSummaryItemType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case final
  case pending
}
@available(iOS 8.0, *)
class PKPaymentSummaryItem : NSObject {
  convenience init(label label: String, amount amount: NSDecimalNumber)
  @available(iOS 9.0, *)
  convenience init(label label: String, amount amount: NSDecimalNumber, type type: PKPaymentSummaryItemType)
  var label: String
  @NSCopying var amount: NSDecimalNumber
  @available(iOS 9.0, *)
  var type: PKPaymentSummaryItemType
}
@available(iOS 8.0, *)
class PKShippingMethod : PKPaymentSummaryItem {
  var identifier: String?
  var detail: String?
}
@available(iOS 8.0, *)
class PKPaymentRequest : NSObject {
  var merchantIdentifier: String
  var countryCode: String
  var supportedNetworks: [String]
  var merchantCapabilities: PKMerchantCapability
  var paymentSummaryItems: [PKPaymentSummaryItem]
  var currencyCode: String
  var requiredBillingAddressFields: PKAddressField
  @available(iOS, introduced: 8.0, deprecated: 9.0, message: "Use billingContact instead")
  unowned(unsafe) var billingAddress: @sil_unmanaged ABRecord?
  @available(iOS 9.0, *)
  var billingContact: PKContact?
  var requiredShippingAddressFields: PKAddressField
  @available(iOS, introduced: 8.0, deprecated: 9.0, message: "Use shippingContact instead")
  unowned(unsafe) var shippingAddress: @sil_unmanaged ABRecord?
  @available(iOS 9.0, *)
  var shippingContact: PKContact?
  var shippingMethods: [PKShippingMethod]?
  @available(iOS 8.3, *)
  var shippingType: PKShippingType
  @NSCopying var applicationData: NSData?
}
