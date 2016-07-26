
@available(OSX 10.10, *)
enum LAError : Int {
  case authenticationFailed
  case userCancel
  case userFallback
  case systemCancel
  case passcodeNotSet
  case touchIDNotAvailable
  case touchIDNotEnrolled
  @available(OSX 10.11, *)
  case touchIDLockout
  @available(OSX 10.11, *)
  case appCancel
  @available(OSX 10.11, *)
  case invalidContext
}

extension LAError : _BridgedNSError {
}
@available(OSX 10.10, *)
let LAErrorDomain: String
