
let MCErrorDomain: String
@available(iOS 7.0, *)
enum MCErrorCode : Int {
  case unknown
  case notConnected
  case invalidParameter
  case unsupported
  case timedOut
  case cancelled
  case unavailable
}

@available(OSX 10.10, iOS 7.0, *)
extension MCErrorCode : _BridgedNSError {
}
