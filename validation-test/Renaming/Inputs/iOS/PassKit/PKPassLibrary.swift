
@available(iOS 7.0, *)
enum PKPassLibraryAddPassesStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case didAddPasses
  case shouldReviewPasses
  case didCancelAddPasses
}
@available(iOS 9.0, *)
enum PKAutomaticPassPresentationSuppressionResult : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case notSupported
  case alreadyPresenting
  case denied
  case cancelled
  case success
}
typealias PKSuppressionRequestToken = Int
@available(iOS 6.0, *)
class PKPassLibrary : NSObject {
  @available(iOS 6.0, *)
  @discardableResult
  class func isPassLibraryAvailable() -> Bool
  @available(iOS 9.0, *)
  @discardableResult
  class func requestAutomaticPassPresentationSuppression(responseHandler responseHandler: (PKAutomaticPassPresentationSuppressionResult) -> Void) -> PKSuppressionRequestToken
  @available(iOS 9.0, *)
  class func endAutomaticPassPresentationSuppression(withRequestToken requestToken: PKSuppressionRequestToken)
  @available(iOS 9.0, *)
  @discardableResult
  class func isSuppressingAutomaticPassPresentation() -> Bool
  @available(iOS, introduced: 8.0, deprecated: 9.0, message: "Use -[PKPassLibrary isPaymentPassActivationAvailable] instead")
  @discardableResult
  class func isPaymentPassActivationAvailable() -> Bool
  @available(iOS 9.0, *)
  @discardableResult
  func isPaymentPassActivationAvailable() -> Bool
  @discardableResult
  func passes() -> [PKPass]
  @discardableResult
  func pass(withPassTypeIdentifier identifier: String, serialNumber serialNumber: String) -> PKPass?
  @available(iOS 8.0, *)
  @discardableResult
  func passes(of passType: PKPassType) -> [PKPass]
  @available(iOS 9.0, *)
  @discardableResult
  func remotePaymentPasses() -> [PKPaymentPass]
  func removePass(_ pass: PKPass)
  @discardableResult
  func containsPass(_ pass: PKPass) -> Bool
  @discardableResult
  func replacePass(with pass: PKPass) -> Bool
  @available(iOS 7.0, *)
  func addPasses(_ passes: [PKPass], withCompletionHandler completion: ((PKPassLibraryAddPassesStatus) -> Void)? = nil)
  @available(iOS 8.3, *)
  func openPaymentSetup()
  @available(iOS 9.0, *)
  @discardableResult
  func canAddPaymentPass(withPrimaryAccountIdentifier primaryAccountIdentifier: String) -> Bool
  @available(iOS 8.0, *)
  func activate(_ paymentPass: PKPaymentPass, withActivationData activationData: NSData, completion completion: ((Bool, NSError) -> Void)? = nil)
  @available(iOS, introduced: 8.0, deprecated: 9.0, message: "Use activatePaymentPass:withActivationData:completion: instead")
  func activate(_ paymentPass: PKPaymentPass, withActivationCode activationCode: String, completion completion: ((Bool, NSError) -> Void)? = nil)
}
@available(iOS 6.0, *)
let PKPassLibraryDidChangeNotification: String
@available(iOS 9.0, *)
let PKPassLibraryRemotePaymentPassesDidChangeNotification: String
@available(iOS 6.0, *)
let PKPassLibraryAddedPassesUserInfoKey: String
@available(iOS 6.0, *)
let PKPassLibraryReplacementPassesUserInfoKey: String
@available(iOS 6.0, *)
let PKPassLibraryRemovedPassInfosUserInfoKey: String
@available(iOS 6.0, *)
let PKPassLibraryPassTypeIdentifierUserInfoKey: String
@available(iOS 6.0, *)
let PKPassLibrarySerialNumberUserInfoKey: String
