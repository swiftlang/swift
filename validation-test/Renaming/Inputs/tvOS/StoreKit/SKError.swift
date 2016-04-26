
@available(tvOS 3.0, *)
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
  @available(tvOS 9.3, *)
  case cloudServicePermissionDenied
  @available(tvOS 9.3, *)
  case cloudServiceNetworkConnectionFailed
}
