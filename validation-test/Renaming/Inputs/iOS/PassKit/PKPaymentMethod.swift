
@available(iOS 9.0, *)
class PKPaymentMethod : NSObject {
  var displayName: String? { get }
  var network: String? { get }
  var type: PKPaymentMethodType { get }
  var paymentPass: PKPaymentPass? { get }
}
@available(iOS 9.0, *)
struct PKPaymentMethodType : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var debit: PKPaymentMethodType { get }
  static var credit: PKPaymentMethodType { get }
  static var prepaid: PKPaymentMethodType { get }
  static var store: PKPaymentMethodType { get }
}
