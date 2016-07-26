
@available(watchOS 2.0, *)
let PKPassKitErrorDomain: String
@available(watchOS 2.0, *)
enum PKPassKitErrorCode : Int {
  case unknownError
  case invalidDataError
  case unsupportedVersionError
  case invalidSignature
  case notEntitledError
}

@available(iOS 6.0, *)
extension PKPassKitErrorCode : _BridgedNSError {
}
