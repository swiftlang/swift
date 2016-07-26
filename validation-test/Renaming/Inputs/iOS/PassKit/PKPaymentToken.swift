
@available(iOS 8.0, *)
class PKPaymentToken : NSObject {
  @available(iOS 9.0, *)
  var paymentMethod: PKPaymentMethod { get }
  @available(iOS, introduced: 8.0, deprecated: 9.0, message: "Use paymentMethod instead")
  var paymentInstrumentName: String { get }
  @available(iOS, introduced: 8.0, deprecated: 9.0, message: "Use paymentMethod instead")
  var paymentNetwork: String { get }
  var transactionIdentifier: String { get }
  var paymentData: NSData { get }
}
