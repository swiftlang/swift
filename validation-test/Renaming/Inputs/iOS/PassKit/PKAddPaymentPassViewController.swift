
@available(iOS 9.0, *)
enum PKAddPaymentPassError : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unsupported
  case userCancelled
  case systemCancelled
}
@available(iOS 9.0, *)
class PKAddPaymentPassRequestConfiguration : NSObject {
  init?(encryptionScheme encryptionScheme: String)
  var encryptionScheme: String { get }
  var cardholderName: String?
  var primaryAccountSuffix: String?
  var localizedDescription: String?
  var primaryAccountIdentifier: String?
  var paymentNetwork: String?
}
@available(iOS 9.0, *)
class PKAddPaymentPassRequest : NSObject {
  @NSCopying var encryptedPassData: NSData?
  @NSCopying var activationData: NSData?
  @NSCopying var ephemeralPublicKey: NSData?
  @NSCopying var wrappedKey: NSData?
}
protocol PKAddPaymentPassViewControllerDelegate : NSObjectProtocol {
  @available(iOS 9.0, *)
  func addPaymentPassViewController(_ controller: PKAddPaymentPassViewController, generateRequestWithCertificateChain certificates: [NSData], nonce nonce: NSData, nonceSignature nonceSignature: NSData, completionHandler handler: (PKAddPaymentPassRequest) -> Void)
  @available(iOS 9.0, *)
  func addPaymentPassViewController(_ controller: PKAddPaymentPassViewController, didFinishAdding pass: PKPaymentPass?, error error: NSError?)
}
@available(iOS 9.0, *)
class PKAddPaymentPassViewController : UIViewController {
  @discardableResult
  class func canAddPaymentPass() -> Bool
  init?(requestConfiguration configuration: PKAddPaymentPassRequestConfiguration, delegate delegate: PKAddPaymentPassViewControllerDelegate?)
  weak var delegate: @sil_weak PKAddPaymentPassViewControllerDelegate?
}
