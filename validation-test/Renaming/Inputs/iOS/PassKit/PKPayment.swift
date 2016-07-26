
@available(iOS 8.0, *)
class PKPayment : NSObject {
  var token: PKPaymentToken { get }
  @available(iOS, introduced: 8.0, deprecated: 9.0, message: "Use billingContact instead")
  var billingAddress: ABRecord? { get }
  @available(iOS 9.0, *)
  var billingContact: PKContact? { get }
  @available(iOS, introduced: 8.0, deprecated: 9.0, message: "Use shippingContact instead")
  var shippingAddress: ABRecord? { get }
  @available(iOS 9.0, *)
  var shippingContact: PKContact? { get }
  var shippingMethod: PKShippingMethod? { get }
}
