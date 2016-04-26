
@available(iOS 3.0, *)
let SKErrorDomain: String
enum SKErrorCode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case clientInvalid
  case paymentCancelled
  case paymentInvalid
  case paymentNotAllowed
  case storeProductNotAvailable
  @available(iOS 9.3, *)
  case cloudServicePermissionDenied
  @available(iOS 9.3, *)
  case cloudServiceNetworkConnectionFailed
}
