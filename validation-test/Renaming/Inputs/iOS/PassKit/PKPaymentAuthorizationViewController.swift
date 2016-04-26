
@available(iOS 8.0, *)
enum PKPaymentAuthorizationStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case success
  case failure
  case invalidBillingPostalAddress
  case invalidShippingPostalAddress
  case invalidShippingContact
  @available(iOS 9.2, *)
  case pinRequired
  @available(iOS 9.2, *)
  case pinIncorrect
  @available(iOS 9.2, *)
  case pinLockout
}
protocol PKPaymentAuthorizationViewControllerDelegate : NSObjectProtocol {
  @available(iOS 8.0, *)
  func paymentAuthorizationViewController(_ controller: PKPaymentAuthorizationViewController, didAuthorizePayment payment: PKPayment, completion completion: (PKPaymentAuthorizationStatus) -> Void)
  @available(iOS 8.0, *)
  func paymentAuthorizationViewControllerDidFinish(_ controller: PKPaymentAuthorizationViewController)
  @available(iOS 8.3, *)
  optional func paymentAuthorizationViewControllerWillAuthorizePayment(_ controller: PKPaymentAuthorizationViewController)
  @available(iOS 8.0, *)
  optional func paymentAuthorizationViewController(_ controller: PKPaymentAuthorizationViewController, didSelect shippingMethod: PKShippingMethod, completion completion: (PKPaymentAuthorizationStatus, [PKPaymentSummaryItem]) -> Void)
  @available(iOS, introduced: 8.0, deprecated: 9.0, message: "Use the CNContact backed delegate method instead")
  optional func paymentAuthorizationViewController(_ controller: PKPaymentAuthorizationViewController, didSelectShippingAddress address: ABRecord, completion completion: (PKPaymentAuthorizationStatus, [PKShippingMethod], [PKPaymentSummaryItem]) -> Void)
  @available(iOS 9.0, *)
  optional func paymentAuthorizationViewController(_ controller: PKPaymentAuthorizationViewController, didSelectShippingContact contact: PKContact, completion completion: (PKPaymentAuthorizationStatus, [PKShippingMethod], [PKPaymentSummaryItem]) -> Void)
  @available(iOS 9.0, *)
  optional func paymentAuthorizationViewController(_ controller: PKPaymentAuthorizationViewController, didSelect paymentMethod: PKPaymentMethod, completion completion: ([PKPaymentSummaryItem]) -> Void)
}
@available(iOS 8.0, *)
class PKPaymentAuthorizationViewController : UIViewController {
  @discardableResult
  class func canMakePayments() -> Bool
  @discardableResult
  class func canMakePayments(usingNetworks supportedNetworks: [String]) -> Bool
  @available(iOS 9.0, *)
  @discardableResult
  class func canMakePayments(usingNetworks supportedNetworks: [String], capabilities capabilties: PKMerchantCapability) -> Bool
  unowned(unsafe) var delegate: @sil_unmanaged PKPaymentAuthorizationViewControllerDelegate?
  init(paymentRequest request: PKPaymentRequest)
}
