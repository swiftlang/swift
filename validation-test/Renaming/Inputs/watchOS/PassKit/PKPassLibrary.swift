
@available(watchOS 2.0, *)
enum PKPassLibraryAddPassesStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case didAddPasses
  case shouldReviewPasses
  case didCancelAddPasses
}
typealias PKSuppressionRequestToken = Int
@available(watchOS 2.0, *)
class PKPassLibrary : NSObject {
  @available(watchOS 2.0, *)
  @discardableResult
  class func isPassLibraryAvailable() -> Bool
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use -[PKPassLibrary isPaymentPassActivationAvailable] instead")
  @discardableResult
  class func isPaymentPassActivationAvailable() -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func isPaymentPassActivationAvailable() -> Bool
  @discardableResult
  func passes() -> [PKPass]
  @discardableResult
  func pass(withPassTypeIdentifier identifier: String, serialNumber serialNumber: String) -> PKPass?
  @available(watchOS 2.0, *)
  @discardableResult
  func passes(of passType: PKPassType) -> [PKPass]
  @available(watchOS 2.0, *)
  @discardableResult
  func remotePaymentPasses() -> [PKPaymentPass]
  func removePass(_ pass: PKPass)
  @discardableResult
  func containsPass(_ pass: PKPass) -> Bool
  @discardableResult
  func replacePass(with pass: PKPass) -> Bool
  @available(watchOS 2.0, *)
  func addPasses(_ passes: [PKPass], withCompletionHandler completion: ((PKPassLibraryAddPassesStatus) -> Void)? = nil)
  @available(watchOS 2.0, *)
  @discardableResult
  func canAddPaymentPass(withPrimaryAccountIdentifier primaryAccountIdentifier: String) -> Bool
}
@available(watchOS 2.0, *)
let PKPassLibraryDidChangeNotification: String
@available(watchOS 2.0, *)
let PKPassLibraryRemotePaymentPassesDidChangeNotification: String
@available(watchOS 2.0, *)
let PKPassLibraryAddedPassesUserInfoKey: String
@available(watchOS 2.0, *)
let PKPassLibraryReplacementPassesUserInfoKey: String
@available(watchOS 2.0, *)
let PKPassLibraryRemovedPassInfosUserInfoKey: String
@available(watchOS 2.0, *)
let PKPassLibraryPassTypeIdentifierUserInfoKey: String
@available(watchOS 2.0, *)
let PKPassLibrarySerialNumberUserInfoKey: String
