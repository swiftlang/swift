
@available(watchOS 2.0, *)
let WCErrorDomain: String
@available(watchOS 2.0, *)
enum WCErrorCode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case genericError
  case sessionNotSupported
  case sessionMissingDelegate
  case sessionNotActivated
  case deviceNotPaired
  case watchAppNotInstalled
  case notReachable
  case invalidParameter
  case payloadTooLarge
  case payloadUnsupportedTypes
  case messageReplyFailed
  case messageReplyTimedOut
  case fileAccessDenied
  case deliveryFailed
  case insufficientSpace
  @available(watchOS 2.2, *)
  case sessionInactive
  @available(watchOS 2.2, *)
  case transferTimedOut
}
